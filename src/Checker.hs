module Checker where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf

newtype LVar
  = LVar Int
  deriving (Eq, Show, Ord)

data Type
  = Int
  | Bool
  | Function (Type, Type)
  | TVar LVar
  | Undefined -- alternatively, "bottom" or ⊥
  deriving (Eq, Show)

data Term
  = IntLiteral
  | BoolLiteral
  | Conditional (Term, Term, Term)
  | Lambda (String, Term)
  | Variable String
  | Application (Term, Term)
  deriving (Eq, Show)

type Context = Map String Type

data AppState = AppState
  { appCounter :: Int,
    appConstraints :: [(Type, Type)]
  }
  deriving (Show)

newLVar :: State AppState LVar
newLVar = do
  s <- get
  let next = appCounter s + 1
  put s {appCounter = next}
  return (LVar next)

unify :: Type -> Type -> State AppState ()
unify t1 t2 = do
  s <- get
  put s {appConstraints = (t1, t2) : appConstraints s}
  return ()

infer :: Context -> Term -> State AppState Type
infer _ IntLiteral = pure Int
infer _ BoolLiteral = pure Bool
infer ctx (Conditional (cond, term1, term2)) =
  do
    ct <- infer ctx cond
    unify ct Bool
    t1 <- infer ctx term1
    t2 <- infer ctx term2
    unify t1 t2
    return t1
infer ctx (Lambda (arg, body)) =
  do
    lvar <- newLVar
    let t1 = TVar lvar
    let ctx' = Map.insert arg t1 ctx
    t2 <- infer ctx' body
    return (Function (t1, t2))
infer ctx (Variable name) =
  pure $ Map.findWithDefault Undefined name ctx
infer ctx (Application (fn, arg)) =
  do
    ft <- infer ctx fn
    at <- infer ctx arg
    lvar1 <- newLVar
    lvar2 <- newLVar
    let t1 = TVar lvar1
    let t2 = TVar lvar2
    unify t1 at
    unify ft (Function (t1, t2))
    return t2

solve :: Type -> Type -> Map LVar Type -> Either String (Map LVar Type)
solve t1 t2 subst | t1 == t2 = Right subst
solve (TVar v) t subst = solveVar v t subst
solve t (TVar v) subst = solveVar v t subst
solve (Function (in1, out1)) (Function (in2, out2)) subst =
  solve in1 in2 subst >>= solve out1 out2
solve t1 t2 _subst = Left (printf "Could not unify %s with %s" (show t1) (show t2))

occursCheck :: LVar -> Type -> Map LVar Type -> Bool
occursCheck v t subst =
  case t of
    TVar v' | v == v' -> True
    TVar v' ->
      case Map.lookup v' subst of
        Just t' -> occursCheck v t' subst
        Nothing -> False
    Function (t1, t2) ->
      occursCheck v t1 subst || occursCheck v t2 subst
    _ -> False

-- NOTE: Recursively try to resolve variables on the LHS and RHS of the equation
solveVar :: LVar -> Type -> Map LVar Type -> Either String (Map LVar Type)
solveVar v t subst =
  let res1 = Map.lookup v subst
      res2 =
        case t of
          TVar v' -> Map.lookup v' subst
          _ -> Nothing
   in case (res1, res2) of
        (Just t', _) -> solve t' t subst
        (_, Just t') -> solve (TVar v) t' subst
        _ | occursCheck v t subst ->
          Left (printf "Self-reference: %s %s" (show v) (show t))
        _ -> Right $ Map.insert v t subst

check :: Term -> (Type, AppState)
check term =
  let ctx = Map.empty
      initalState =
        AppState
          { appConstraints = [],
            appCounter = 0
          }
   in runState (infer ctx term) initalState
