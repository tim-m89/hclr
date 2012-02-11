{-# LANGUAGE TemplateHaskell, DoAndIfThenElse #-}

module Foreign.HCLR (
  
  module Foreign.Ptr,
  module Foreign.C.String,
  module System.Environment,
  runClr
) where

import Foreign.HCLR.Binding
import Foreign.HCLR.Parser
import Foreign.HCLR.CodeGen
import Foreign.Ptr
import Foreign.C.String
import System.Environment

import Data.Either
import Data.List
import Data.String.HT
import Text.Parsec.Prim
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

runClr :: QuasiQuoter
runClr = QuasiQuoter { quoteExp = parseExpQ
                     , quotePat = parsePatQ
                     , quoteType = parseTypeQ
                     , quoteDec = parseDecQ}


parseExpQ :: String -> Q Exp
parseExpQ str = do
  parsed <- returnQ $ partitionEithers $ map (\l-> case (parse parseStmt "1" l) of
    Left a -> Left l
    Right b -> Right b) (filter (\l-> not $ null $ trim l) (lines str))
  eexp <- runIO (if (null $ fst parsed) then
    compile (snd parsed)
  else
    fail $ foldl' (\a-> \s-> a ++ "A Syntax error occured \"" ++ s ++ "\"") "" (fst parsed))
  case eexp of
    Left s -> fail s
    Right exp -> return exp
    

parsePatQ :: String -> Q Pat
parsePatQ s = do
  runIO $ putStrLn $ "Pat" ++ s
  returnQ $ VarP (mkName s)

parseTypeQ :: String -> Q Type
parseTypeQ s =  do
  runIO $ putStrLn $ "Type" ++ s
  returnQ $ VarT (mkName s)

parseDecQ :: String -> Q [Dec]
parseDecQ s = do
  runIO $ putStrLn $ "Dec" ++ s
  returnQ [FunD (mkName s) []]


