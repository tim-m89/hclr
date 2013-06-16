module Foreign.HCLR.CodeGen where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.List (concat, intersperse)
import Data.Maybe (maybe)
import Data.Tuple (fst, snd)
import Foreign.HCLR.Ast
import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.Map as Map
import Data.String.HT
import Foreign.HCLR.Binding

typeAssem :: CLRType -> IO Assembly
typeAssem = undefined

type TypeAssemMap = Map.Map CLRType Assembly
type SymbolTypeMap = Map.Map String CLRType
type CompilerInfo = (TypeAssemMap, SymbolTypeMap)
type Compiler = StateT CompilerInfo IO

initialCompileState = (Map.empty, Map.empty)

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
  y <- evalStateT  (mapM doStmt x) initialCompileState
  case (sequence y) of
    Right y' -> do
      let z = y' ++  [TH.NoBindS ( (TH.VarE (TH.mkName "return") ) `TH.AppE` (TH.TupE []) )]
      return $ Right $ (TH.VarE (TH.mkName "withRuntime") ) `TH.AppE`  (TH.DoE z) -- [TH.NoBindS $ (TH.VarE (TH.mkName "return") ) `TH.AppE` (TH.TupE [])]



doStmt :: Stmt -> Compiler (Either String TH.Stmt)
doStmt s = case s of
  NoBindStmt exp -> doExp exp >>= \e-> either (return . Left) (return . Right . TH.NoBindS) e
  BindStmt (Symbol name) exp -> do
    modify (\(a,b)-> (a, Map.insert name undefined b))
    doExp exp >>= \e-> either (return . Left) (return . Right . TH.BindS (TH.VarP $ TH.mkName name)) e



doExp :: Exp -> Compiler (Either String TH.Exp)
doExp e =  case e of
  New typ args -> undefined
  Invoke typ mth (Args args) -> do
    liftIO $ putStrLn $ show typ
    liftIO $ putStrLn $ show mth
    args' <- doArgs args
    argTypes <- doArgTypes args
    return $ Right $ (TH.VarE (TH.mkName "invokeMethodCorlib") ) `TH.AppE` (quoteVar typ) `TH.AppE` (doMethodName mth argTypes) `TH.AppE` (TH.ConE (TH.mkName "NullObject") ) `TH.AppE` args'

doArg :: Arg -> Compiler TH.Exp
doArg a = case a of
  ArgStringLit (StringLiteral s) -> return $ TH.LitE $ TH.StringL s
  ArgSym (Symbol s) -> return $ TH.VarE $ TH.mkName s

doArgType :: Arg -> Compiler (Either String String)
doArgType a = case a of
  ArgStringLit (StringLiteral s) -> return $ Right "string"
  ArgSym (Symbol s) -> do
    symTypes <- get >>= return . snd 
    return $ maybe (Left $ "Unknown symbol " ++ s) (Right . show) $ Map.lookup s symTypes

doArgs :: [Arg] -> Compiler TH.Exp
doArgs a = mapM (doArg) a >>= \l-> return $ TH.TupE l

doArgTypes :: [Arg] -> Compiler String
doArgTypes a = do
  l <- mapM (doArgType) a
  case (sequence l) of
    Left l -> undefined
    Right r -> return $ "(" ++ (concat (intersperse "," r)) ++ ")"

quoteVar :: (Show a) => a -> TH.Exp
quoteVar x = TH.LitE $ TH.StringL $ show x

doMethodName mth args = TH.LitE $ TH.StringL $ (show mth) ++ args


