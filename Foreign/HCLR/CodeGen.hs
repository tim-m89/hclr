module Foreign.HCLR.CodeGen where

import Control.Monad
import Foreign.HCLR.Ast
import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.Map as Map
import Data.String.HT
import Foreign.HCLR.Mono

data OpCode
  = OpNop
  | OpBreak
  | OpLoadArg0
  | OpLoadArg1
  | OpLoadArg2
  | OpLoadArg3


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
  return $ Right $ (TH.VarE (TH.mkName "return") ) `TH.AppE` (TH.TupE [])

