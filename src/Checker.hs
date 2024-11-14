module Checker where

import Data.Map (Map)
import qualified Data.Map as Map

data Type
  = Int
  | Bool
  | Function (Type, Type)
  deriving (Eq, Show)

data Term
  = IntLiteral
  | BoolLiteral
  | Conditional (Term, Term, Term)
  | Lambda ((Type, Type), String, Term)
  | Variable String
  deriving (Eq, Show)

type Context = Map String Type

infer :: Context -> Term -> Maybe Type
infer _ IntLiteral = Just Int
infer _ BoolLiteral = Just Bool
infer ctx (Conditional (cond, term1, term2)) =
  if infer ctx cond == Just Bool
    then case (infer ctx term1, infer ctx term2) of
      (Just t1, Just t2) | t1 == t2 -> Just t1
      _ -> Nothing
    else Nothing
infer ctx (Variable name) =
  Map.lookup name ctx
infer ctx (Lambda ((input, output), arg, body)) =
  let ctx' = Map.insert arg input ctx in
    if infer ctx' body == Just output
      then Just (Function (input, output))
      else Nothing
