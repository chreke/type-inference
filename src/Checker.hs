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
  | Application (Term, Term)
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
infer ctx (Lambda ((t1, t2), arg, body)) =
  let ctx' = Map.insert arg t1 ctx in
    if infer ctx' body == Just t2
      then Just (Function (t1, t2))
      else Nothing
infer ctx (Application (fn, arg)) =
  case infer ctx fn of
    Just (Function (t1, t2)) ->
      if infer ctx arg == Just t1
        then Just t2
        else Nothing
    _ -> Nothing
