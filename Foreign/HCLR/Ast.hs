module Foreign.HCLR.Ast where

import Data.List

newtype StringLiteral = StringLiteral String
instance Show StringLiteral where
  show (StringLiteral s) = "\"" ++ s ++ "\" "
newtype Symbol = Symbol String
instance Show Symbol where
  show (Symbol s) = s ++ " "
data Arg = ArgStringLit StringLiteral | ArgSym Symbol
instance Show Arg where
  show (ArgStringLit x) = show x
  show (ArgSym x) = show x
newtype CLRType = CLRType [String]
instance Show CLRType where
  show (CLRType s) = concat $ intersperse "." s
newtype CLRMethod = CLRMethod String
instance Show CLRMethod where
  show (CLRMethod s) = "." ++ s
newtype Args = Args [Arg]
instance Show Args where
  show (Args x) = " Args: " ++ (concatMap show x)
data Exp = New CLRType Args | Invoke CLRType CLRMethod Args
instance Show Exp where
  show (New clrtype args) = "New (" ++ (show clrtype) ++ (show args) ++ ")"
  show (Invoke clrtype mth args) = "Invoke (" ++ (show clrtype) ++ (show mth) ++ (show args) ++ ")"
data Stmt = BindStmt Symbol Exp | NoBindStmt Exp
instance Show Stmt where
  show (BindStmt sym exp) = "BindStmt (" ++ (show sym) ++ "<- " ++ (show exp) ++ ")"
  show (NoBindStmt exp) = "NoBindStmt (" ++ (show exp) ++ ")"

