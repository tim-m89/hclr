module Foreign.HCLR.CodeGen where

import Control.Monad
import Foreign.HCLR.Ast
import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.Map as Map
import Data.String.HT
import Foreign.HCLR.Binding

typeAssem :: CLRType -> IO Assembly
typeAssem = undefined

type TypeAssemMap = Map.Map CLRType Assembly

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
  let types = map stmtGetType x
  typeAssemMap <- getTypeAssemMap types
  let doStmt = stmtr typeAssemMap
  z <- mapM doStmt x 
  return $ Right $ TH.DoE $ z

stmtr :: TypeAssemMap -> Stmt -> IO TH.Stmt
stmtr tam s = case s of
  BindStmt (Symbol name) exp -> expr tam exp >>= \e-> return $ TH.BindS (TH.VarP (TH.mkName name) )  e
  _ -> return $ TH.NoBindS $ (TH.VarE (TH.mkName "return") ) `TH.AppE` (TH.TupE [])

expr :: TypeAssemMap -> Exp -> IO TH.Exp
expr tam e = case e of
  New typ args -> undefined
  Invoke typ mth args -> undefined

