module Foreign.HCLR.CodeGen where

import Foreign.HCLR.Ast
import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.Map as Map

data OpCode
  = OpNop
  | OpBreak
  | OpLoadArg0
  | OpLoadArg1
  | OpLoadArg2
  | OpLoadArg3

newtype Assembly = Assembly String

typeAssem :: CLRType -> IO Assembly
typeAssem = undefined

type TypeAssemMap = Map.Map CLRType Assembly

-- compile takes a list of statements and returns either a compilation error or the haskell ast to load up the code
compile :: [Stmt] -> IO (Either String TH.Exp)
compile x = do
  putStrLn "Compiling HCLR" 
  return $ Right $ (TH.VarE (TH.mkName "return") ) `TH.AppE` (TH.TupE [])

