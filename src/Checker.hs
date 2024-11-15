module Checker where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

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

check :: Term -> (Type, AppState)
check term =
  let ctx = Map.empty
      initalState = AppState {
        appConstraints = [],
        appCounter = 0
      }
  in runState (infer ctx term) initalState
