module Foreign.HCLR.Parser where

import Data.String.HT
import Foreign.HCLR.Ast
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String


parseStringLiteral :: Parser StringLiteral
parseStringLiteral = do
  char '"'
  s <- ((try $ char '\\' >> anyChar) <|> anyChar) `manyTill` (char '"')
  return $ StringLiteral s

parseSymbol :: Parser Symbol
parseSymbol = do
  sym <- many (char '_' <|> alphaNum)
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
  typ <- (many (char '_' <|> alphaNum)) `sepBy` (char '.')
  args <- parseArgs
  return $ New (CLRType typ) args

parseInvoke :: Parser Exp
parseInvoke = do
  strings <- (many (char '_' <|> alphaNum)) `sepBy` (char '.')
  let typ = init strings
  let mth = last strings
  args <- parseArgs
  return $ Invoke (CLRType typ) (CLRMethod mth) args

parseExp :: Parser Exp
parseExp = spaces >> (try parseNew <|> parseInvoke)

parseBindStmt :: Parser Stmt
parseBindStmt = do
  sym <- anyChar `manyTill` bindSymbol >>= return . trim
  exp <- parseExp
  return $ BindStmt (Symbol sym) exp

parseNoBindStmt :: Parser Stmt
parseNoBindStmt = do
  exp <- parseExp
  return $ NoBindStmt exp

parseStmt :: Parser Stmt
parseStmt = spaces >> (try parseBindStmt <|> parseNoBindStmt)

bindSymbol :: Parser String
bindSymbol = try . string $ "<-"

