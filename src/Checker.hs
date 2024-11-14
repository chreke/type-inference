module Checker where

data Type
  = Int
  | Bool
  deriving (Eq, Show)

data Term
  = IntLiteral
  | BoolLiteral
  | Conditional (Term, Term, Term)
  deriving (Eq, Show)

infer :: Term -> Maybe Type
infer IntLiteral = Just Int
infer BoolLiteral = Just Bool
infer (Conditional (cond, term1, term2)) =
  if infer cond == Just Bool
    then case (infer term1, infer term2) of
      (Just t1, Just t2) | t1 == t2 -> Just t1
      _ -> Nothing
    else Nothing
