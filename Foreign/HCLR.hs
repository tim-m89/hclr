{-# LANGUAGE TemplateHaskell, DoAndIfThenElse #-}

module Foreign.HCLR (
  runClr,
  withRuntime,
  invokeMethod,
  newObject,
  Object (..)
) where

import Foreign.HCLR.Parser
import Foreign.HCLR.CodeGen
import Foreign.HCLR.Binding

import Data.Either
import Data.String.HT (trim)
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.Error
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
  loc <- location
  let lineStart = fst $ loc_start loc
  parsed <- return . snd $ foldl (\(n,p)-> \s-> case (trim s) of
    [] -> (n+1,p)
    (x:xs) -> (n+1,(parse (parseStmtLine n) "1" (x:xs)):p)
     ) (lineStart,[]) (lines str)
  let errMsg = foldr (\e-> \s-> s ++ "Syntax error on line " ++ (show $ sourceLine $ errorPos e) ++ "\n") "In: [runClr|...\n" (lefts parsed)
  if (length errMsg > 16) then
    fail errMsg
  else do
    eexp <- runIO $ compile (rights parsed)
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


