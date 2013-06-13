module Foreign.HCLR.CodeGen where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Foreign.HCLR.Ast
import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.Map as Map
import Data.String.HT
import Foreign.HCLR.Binding

typeAssem :: CLRType -> IO Assembly
typeAssem = undefined

type TypeAssemMap = Map.Map CLRType Assembly
type CompileState = StateT TypeAssemMap IO

getTypeAssemMap :: [CLRType] -> IO TypeAssemMap
getTypeAssemMap types = do
  assems <- referencedAssems
  foldM (\m-> \a-> foldM (\m-> \t-> assemHasType a t >>= \hasType-> if hasType then (return $ Map.insert t a m) else (return m)) m types ) Map.empty assems

referencedAssems :: IO [Assembly]
referencedAssems = readFile "assemRefs.txt" >>= \x-> mapM (return . Assembly) $ filter (not . null) $ map trim (lines x)

-- compile takes a list of statements and returns either a compilation error or the haskell ast to load up the code
compile :: [Stmt] -> IO (Either String TH.Exp)
compile x = withRuntime $ do
  putStrLn "Compiling HCLR" 
  mapM (putStrLn . show) x
  y <- evalStateT  (mapM doStmt x) Map.empty
  let z = y ++  [TH.NoBindS ( (TH.VarE (TH.mkName "return") ) `TH.AppE` (TH.TupE []) )]
  return $ Right $ (TH.VarE (TH.mkName "withRuntime") ) `TH.AppE`  (TH.DoE z) -- [TH.NoBindS $ (TH.VarE (TH.mkName "return") ) `TH.AppE` (TH.TupE [])]



doStmt :: Stmt -> CompileState TH.Stmt
doStmt s = case s of
  BindStmt (Symbol name) exp -> return $ TH.NoBindS $ (TH.VarE (TH.mkName "return") ) `TH.AppE` (TH.TupE [])
  NoBindStmt exp -> doExp exp >>= \e-> return $ TH.NoBindS e



doExp :: Exp -> CompileState TH.Exp
doExp e =  case e of
  New typ args -> undefined
  Invoke typ mth (Args args) -> do
    liftIO $ putStrLn $ show typ
    liftIO $ putStrLn $ show mth
    args' <- doArgs args
    return $ (TH.VarE (TH.mkName "invokeMethodCorlib") ) `TH.AppE` (quoteVar typ) `TH.AppE` (quoteVar mth) `TH.AppE` (TH.ConE (TH.mkName "NullObject") ) `TH.AppE` args'

doArg :: Arg -> CompileState TH.Exp
doArg a = case a of
  ArgStringLit (StringLiteral s) -> return (TH.LitE $ TH.StringL s)
  ArgSym _ -> undefined

doArgs :: [Arg] -> CompileState TH.Exp
doArgs a = mapM (doArg) a >>= \l-> return $ TH.TupE l

quoteVar :: (Show a) => a -> TH.Exp
quoteVar x = TH.LitE $ TH.StringL $ show x

{-
stmtr :: TypeAssemMap -> Stmt -> IO TH.Stmt
stmtr tam s = case s of
  BindStmt (Symbol name) exp -> expr tam exp >>= \e-> return $ TH.BindS (TH.VarP (TH.mkName name) )  e
  _ -> return $ TH.NoBindS $ (TH.VarE (TH.mkName "return") ) `TH.AppE` (TH.TupE [])

expr :: TypeAssemMap -> Exp -> IO TH.Exp
expr tam e = case e of
  New typ args -> undefined
  Invoke typ mth args -> undefined
-}
