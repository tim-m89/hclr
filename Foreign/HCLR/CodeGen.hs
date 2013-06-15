module Foreign.HCLR.CodeGen where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.List (concat, intersperse)
import Foreign.HCLR.Ast
import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.Map as Map
import Data.String.HT
import Foreign.HCLR.Binding

typeAssem :: CLRType -> IO Assembly
typeAssem = undefined

type TypeAssemMap = Map.Map CLRType Assembly
type Compiler = StateT TypeAssemMap IO

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



doStmt :: Stmt -> Compiler TH.Stmt
doStmt s = case s of
  BindStmt (Symbol name) exp -> return $ TH.NoBindS $ (TH.VarE (TH.mkName "return") ) `TH.AppE` (TH.TupE [])
  NoBindStmt exp -> doExp exp >>= \e-> return $ TH.NoBindS e



doExp :: Exp -> Compiler TH.Exp
doExp e =  case e of
  New typ args -> undefined
  Invoke typ mth (Args args) -> do
    liftIO $ putStrLn $ show typ
    liftIO $ putStrLn $ show mth
    args' <- doArgs args
    argTypes <- doArgTypes args
    return $ (TH.VarE (TH.mkName "invokeMethodCorlib") ) `TH.AppE` (quoteVar typ) `TH.AppE` (doMethodName mth argTypes) `TH.AppE` (TH.ConE (TH.mkName "NullObject") ) `TH.AppE` args'

doArg :: Arg -> Compiler TH.Exp
doArg a = case a of
  ArgStringLit (StringLiteral s) -> return (TH.LitE $ TH.StringL s)
  ArgSym _ -> undefined

doArgType :: Arg -> Compiler String
doArgType a = case a of
  ArgStringLit (StringLiteral s) -> return "string"
  ArgSym _ -> undefined  

doArgs :: [Arg] -> Compiler TH.Exp
doArgs a = mapM (doArg) a >>= \l-> return $ TH.TupE l

doArgTypes :: [Arg] -> Compiler String
doArgTypes a = mapM (doArgType) a >>= \l-> return $ "(" ++ (concat (intersperse "," l)) ++ ")"

quoteVar :: (Show a) => a -> TH.Exp
quoteVar x = TH.LitE $ TH.StringL $ show x

doMethodName mth args = TH.LitE $ TH.StringL $ (show mth) ++ args

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
