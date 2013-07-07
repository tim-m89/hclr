module Foreign.HCLR.Ast where

import Data.List
import qualified Data.Map as Map

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
  show (CLRMethod s) = s
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


stmtGetType :: Stmt -> CLRType
stmtGetType s = case s of
  BindStmt sym e -> expGetType e
  NoBindStmt e -> expGetType e

expGetType :: Exp -> CLRType
expGetType e = case e of
  New t a -> t
  Invoke t m a -> t

newtype Assembly = Assembly String
instance Show Assembly where
  show (Assembly a) = a


instance Eq CLRType where
  (CLRType a) == (CLRType b) = a == b 

instance Ord CLRType where
  compare (CLRType a) (CLRType b) = compare a b

instance Eq Symbol where
  (Symbol a) == (Symbol b) = a == b 

instance Ord Symbol where
  compare (Symbol a) (Symbol b) = compare a b


