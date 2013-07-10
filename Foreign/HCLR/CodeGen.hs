{-# LANGUAGE DoAndIfThenElse, NamedFieldPuns #-}

module Foreign.HCLR.CodeGen (
  compile
) where

import Control.Monad (filterM, forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify)
import Data.List (reverse)
import Data.Maybe (maybe, fromJust)
import Foreign.HCLR.Ast
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Ppr (pprint)
import qualified Data.Map as Map
import Data.String.HT (trim)
import Foreign.HCLR.Binding
import Foreign (nullPtr)

type TypeMap = Map.Map CLRType RuntimeType
type SymbolTypeMap = Map.Map Symbol RuntimeType


data CompilerInfo = CompilerInfo { typeMap :: TypeMap
                                 , symbolTypeMap :: SymbolTypeMap
                                 }

type Compiler = StateT CompilerInfo IO

initialCompileState :: [(CLRType, RuntimeType)] -> CompilerInfo
initialCompileState typImg = CompilerInfo { typeMap = Map.fromList typImg
                                          , symbolTypeMap = Map.empty
                                          }

updateSymbols :: (Symbol, RuntimeType) -> CompilerInfo -> CompilerInfo
updateSymbols (s, r) CompilerInfo {typeMap, symbolTypeMap} = CompilerInfo { typeMap
                                                                          , symbolTypeMap = Map.insert s r symbolTypeMap
                                                                          } 

sameLength :: [a] -> [b] -> Bool
sameLength a b = (length a) == (length b)

expImage :: Exp -> Compiler Image
expImage exp = do
  runType <- expType exp
  liftIO $ typeGetImage runType

expType :: Exp -> Compiler RuntimeType
expType exp = do
  let typ = expGetType exp
  types <- get >>= return . typeMap
  let runType = fromJust $ Map.lookup typ types
  return runType


findMethod :: RuntimeType -> String -> Sig -> IO (Either String Method)
findMethod typ name sig = do
  methods <- typeGetMethods typ name
  if (null methods) then
    return $ Left $ "No method named " ++ name
  else do
    methods2 <- filterM (\method-> do
      methodSig <- methodGetSig method
      if not $ sameLength sig methodSig then
        return False
      else
        isSigCompat sig methodSig) methods 
    case (length methods2) of
      0 -> return $ Left "No method found with matching sig"
      1 -> return $ Right (head methods2)
      _ -> do
        methodsWithExactSig <- filterM (\m-> methodSigIs m sig) methods2
        case (length methodsWithExactSig) of
          0 -> return $ Left "Ambiguous signature"
          1 -> return $ Right (head methodsWithExactSig)
          _ -> return $ Left "Ambiguous signature"



typeFindImage :: [Image] -> CLRType -> IO (Either String (CLRType, RuntimeType))
typeFindImage images typ = do
    t <- mapM (\image-> imageGetType image typ) images 
    let assemsFound = filter (\typ-> typ /= nullPtr) t
    case (length assemsFound) of
      0 -> return $ Left "Type not found in any assem"
      1 -> return $ Right (typ, head assemsFound)
      _ -> return $ Left "Type found in too many assems"

loadImages :: IO (Either String [Image])
loadImages = do
  contents <- readFile "assemRefs.txt"
  let assems = filter (not . null) $ map trim (lines contents)
  images <- forM assems $ \assem-> do
    image <- assemblyImage assem
    if image == nullPtr then
      return $ Left $ "Could not load " ++ assem
    else do
      imageName <- imageGetName image
      putStrLn $ "Loaded \"" ++ imageName ++ "\""
      return $ Right image
  case (sequence images) of
    Left s -> return $ Left s
    Right img -> return $ Right img

-- compile takes a list of statements and returns either a compilation error or the haskell ast to load up the code
compile :: [Stmt] -> IO (Either String TH.Exp)
compile x = withRuntime $ do
  putStrLn "Compiling HCLR"
  images <- loadImages
  case images of
    Left imageLoadError -> return $ Left imageLoadError
    Right loadedImages -> do
      typImages <- mapM (\stmt-> typeFindImage loadedImages $ stmtGetType stmt) x
      case (sequence typImages) of
        Left typFindError -> return $ Left typFindError
        Right tim -> do
          y <- evalStateT  (mapM doStmt (reverse x)) $ initialCompileState tim
          case (sequence y) of
            Left s -> return $ Left s
            Right y' -> do
              let z = y' ++  [TH.NoBindS ( (TH.VarE (TH.mkName "return") ) `TH.AppE` (TH.TupE []) )]
              return $ Right $ (TH.VarE (TH.mkName "withRuntime") ) `TH.AppE`  (TH.DoE z) -- [TH.NoBindS $ (TH.VarE (TH.mkName "return") ) `TH.AppE` (TH.TupE [])]



doStmt :: Stmt -> Compiler (Either String TH.Stmt)
doStmt s = case s of
  NoBindStmt exp -> doExp exp >>= \e-> either (return . Left) (\(ex,_)-> do
    liftIO $ putStrLn $ pprint ex  --debuggin
    return . Right . TH.NoBindS $ ex ) e
  BindStmt sym@(Symbol name) exp -> do
    eitherExp <- doExp exp
    flip (either $ return . Left) eitherExp $ \(thexp,typ)-> do
      modify $ updateSymbols (sym,typ)
      return . Right . TH.BindS (TH.VarP $ TH.mkName name) $ thexp 

doExp :: Exp -> Compiler (Either String (TH.Exp, RuntimeType))
doExp exp = do
  image <- expImage exp
  imageName <- liftIO $ imageGetName image
  t <- expType exp
  esig <- argsGetSig (expGetArgs exp)
  args <- expDoArgs exp
  case esig of
    Left sigError -> return $ Left sigError
    Right sig -> do
    emethod <- liftIO $ findMethod t (expGetCallName exp) sig
    case emethod of
      Left noMethod -> return $ Left noMethod
      Right method -> do
      sigString <- liftIO $ methodGetSigString method
      case exp of
        New typ _ -> do
          return $ Right $ ( (TH.VarE (TH.mkName "newObject") ) `TH.AppE` (TH.LitE $ TH.StringL $ imageName) `TH.AppE` (quoteVar typ) `TH.AppE` args , t)
        Invoke typ _ _ -> do
          methodName <- liftIO $ getMethodName method
          returnType <- liftIO $ methodGetReturnType method
          return $ Right $ ( (TH.VarE (TH.mkName "invokeMethod") ) `TH.AppE` (TH.LitE $ TH.StringL $ imageName) `TH.AppE` (quoteVar typ) `TH.AppE` (TH.LitE $ TH.StringL $ methodName ++ sigString) `TH.AppE` (TH.ConE (TH.mkName "NullObject") ) `TH.AppE` args , returnType)

doArg :: Arg -> Compiler TH.Exp
doArg a = case a of
  ArgStringLit (StringLiteral s) -> return $ TH.LitE $ TH.StringL s
  ArgSym (Symbol s) -> return $ TH.VarE $ TH.mkName s

doArgs :: Args -> Compiler TH.Exp
doArgs (Args a) = mapM (doArg) a >>= \l-> return $ TH.TupE l

expDoArgs :: Exp -> Compiler TH.Exp
expDoArgs exp = case exp of
    New _ args -> doArgs args
    Invoke _ _ args -> doArgs args

argGetType :: Arg -> Compiler (Either String RuntimeType)
argGetType arg = case arg of
  ArgStringLit _ -> liftIO $ stringType >>= return . Right
  ArgSym sym -> do
    symTypes <- get >>= return . symbolTypeMap
    let typ = Map.lookup sym symTypes
    return $ maybe (Left $ "Unknown symbol \"" ++ show sym ++ "\"") Right typ 

argsGetSig :: Args -> Compiler (Either String Sig)
argsGetSig (Args a) = do
  l <- mapM argGetType a
  case (sequence l) of
    Left s -> return $ Left s
    Right sig -> return $ Right sig

quoteVar :: (Show a) => a -> TH.Exp
quoteVar x = TH.LitE $ TH.StringL $ show x



