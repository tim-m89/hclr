module Foreign.HCLR.CodeGen where

import qualified Foreign.HCLR.Ast as HCLRAst
import Language.Haskell.TH.Syntax

-- compile takes a list of statements and returns either a compilation error or the haskell ast to load up the code
compile :: [HCLRAst.Stmt] -> IO (Either String Exp)
compile x = do
  putStrLn "Compiling" 
  return $ Right $ (VarE (mkName "return") ) `AppE` (TupE [])

