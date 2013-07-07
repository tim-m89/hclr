{-# LANGUAGE DoAndIfThenElse #-}

module Foreign.HCLR.Parser where

import Data.String.HT
import Foreign.HCLR.Ast
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.String


parseStringLiteral :: Parser StringLiteral
parseStringLiteral = do
  char '"'
  s <- ((try $ char '\\' >> anyChar) <|> anyChar) `manyTill` (char '"')
  return $ StringLiteral s

parseSymbol :: Parser Symbol
parseSymbol = do
  sym <- many1 (char '_' <|> alphaNum)
  return $ Symbol sym

parseArg :: Parser Arg
parseArg = try (parseStringLiteral >>= return . ArgStringLit) <|> (parseSymbol >>= return . ArgSym)

parseArgs :: Parser Args
parseArgs = do
  spaces
  args <- parseArg `sepBy` space
  return $ Args args

parseNew :: Parser Exp
parseNew = do
  string "new"
  many1 space
  typ <- (many (char '_' <|> alphaNum)) `sepBy1` (char '.')
  args <- parseArgs
  return $ New (CLRType typ) args

parseInvoke :: Parser Exp
parseInvoke = do
  s <- (many (char '_' <|> alphaNum)) `sepBy` (char '.')
  if length s < 2 then
    fail ""
  else do
    let typ = init s
    let mth = last s
    args <- parseArgs
    return $ Invoke (CLRType typ) (CLRMethod mth) args

parseExp :: Parser Exp
parseExp = spaces >> (try parseNew <|> parseInvoke)

parseBindStmt :: Parser Stmt
parseBindStmt = do
  sym <- (char '_' <|> space <|> alphaNum) `manyTill` bindSymbol >>= return . trim
  exp <- parseExp
  return $ BindStmt (Symbol sym) exp

parseNoBindStmt :: Parser Stmt
parseNoBindStmt = do
  exp <- parseExp
  return $ NoBindStmt exp

parseStmt :: Parser Stmt
parseStmt = spaces >> (try parseBindStmt <|> parseNoBindStmt)

parseStmtLine :: Int -> Parser Stmt
parseStmtLine n = setPosition (newPos "1" n 1) >> parseStmt

bindSymbol :: Parser String
bindSymbol = try . string $ "<-"

