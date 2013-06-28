{-# LANGUAGE TupleSections #-}

module Foreign.HCLR.CodeGen where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.List (concat, intersperse)
import Data.Maybe (maybe, fromJust)
import Data.Tuple (fst, snd)
import Foreign.HCLR.Ast
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Ppr (pprint)
import qualified Data.Map as Map
import Data.String.HT
import Foreign.HCLR.Binding


type TypeAssemMap = Map.Map CLRType Assembly
type SymbolTypeMap = Map.Map String CLRType
data CompilerInfo = CompilerInfo { typeAssemMap :: TypeAssemMap
                                 , symbolTypeMap :: SymbolTypeMap
                                 , assemRefs :: [Assembly]
                                 }

type Compiler = StateT CompilerInfo IO

initialCompileState :: [Assembly] -> [(CLRType, Assembly)] -> CompilerInfo
initialCompileState a ta = CompilerInfo { typeAssemMap = Map.fromList ta
                                        , symbolTypeMap = Map.empty
                                        , assemRefs = a
                                        }


gta :: Compiler TypeAssemMap
gta = get >>= return . typeAssemMap

typeAssem :: CLRType -> Compiler Assembly
typeAssem typ = do
  tam <- gta
  return $ fromJust $ Map.lookup typ tam

referencedAssems :: IO [Assembly]
referencedAssems = readFile "assemRefs.txt" >>= \x-> mapM (return . Assembly) $ filter (not . null) $ map trim (lines x)

typeFindAssem :: [Assembly] -> CLRType -> IO (Either String (CLRType, Assembly))
typeFindAssem assems t = do
    assemsFound <- filterM (\a-> assemHasType a t) assems
    case (length assemsFound) of
      0 -> return $ Left "Type not found in any assem"
      1 -> return $ Right (t, head assemsFound)
      _ -> return $ Left "Type found in too many assems"



-- compile takes a list of statements and returns either a compilation error or the haskell ast to load up the code
compile :: [Stmt] -> IO (Either String TH.Exp)
compile x = withRuntime $ do
  putStrLn "Compiling HCLR"
  assems <- referencedAssems
  ta <- mapM (\stmt-> typeFindAssem assems $ stmtGetType stmt) x
  case (sequence ta) of
    Left tas -> return $ Left tas
    Right ta' -> do
      y <- evalStateT  (mapM doStmt x) $ initialCompileState assems ta'
      case (sequence y) of
        Left s -> return $ Left s
        Right y' -> do
          let z = y' ++  [TH.NoBindS ( (TH.VarE (TH.mkName "return") ) `TH.AppE` (TH.TupE []) )]
          return $ Right $ (TH.VarE (TH.mkName "withRuntime") ) `TH.AppE`  (TH.DoE z) -- [TH.NoBindS $ (TH.VarE (TH.mkName "return") ) `TH.AppE` (TH.TupE [])]



doStmt :: Stmt -> Compiler (Either String TH.Stmt)
doStmt s = case s of
  NoBindStmt exp -> doExp exp >>= \e-> either (return . Left) (\ex-> do
    liftIO $ putStrLn $ pprint ex
    return . Right . TH.NoBindS $ ex ) e
  BindStmt (Symbol name) exp -> do
    e <- doExp exp
    flip (either $ return . Left) e $ \thexp-> do
      return . Right . TH.BindS (TH.VarP $ TH.mkName name) $ thexp 



doExp :: Exp -> Compiler (Either String TH.Exp)
doExp e =  case e of
  New typ args -> undefined
  Invoke typ mth (Args args) -> do
    args' <- doArgs args
    argTypes <- doArgTypes args
    assem <- typeAssem typ
    return $ Right $ (TH.VarE (TH.mkName "invokeMethod") ) `TH.AppE` (quoteVar assem) `TH.AppE` (quoteVar typ) `TH.AppE` (doMethodName mth argTypes) `TH.AppE` (TH.ConE (TH.mkName "NullObject") ) `TH.AppE` args'

doArg :: Arg -> Compiler TH.Exp
doArg a = case a of
  ArgStringLit (StringLiteral s) -> return $ TH.LitE $ TH.StringL s
  ArgSym (Symbol s) -> return $ TH.VarE $ TH.mkName s

doArgType :: Arg -> Compiler (Either String String)
doArgType a = case a of
  ArgStringLit (StringLiteral s) -> return $ Right "string"
  ArgSym (Symbol s) -> do
    symTypes <- get >>= return . symbolTypeMap 
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


