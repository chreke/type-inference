module Checker where

data Type
  = Int
  | Bool
  deriving (Eq, Show)

data Term
  = IntLiteral
  | BoolLiteral
  deriving (Eq, Show)

infer :: Term -> Maybe Type
infer IntLiteral = Just Int
infer BoolLiteral = Just Bool
