module Checker where

-- import Control.Monad.State
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

type LVars = [(LVar, Type)]

data State = State {
  stateCounter :: Int,
  stateLVars :: LVars
} deriving Show

initState :: State
initState = State {
  stateCounter = 0,
  stateLVars = []
}

newLVar :: State -> (LVar, State)
newLVar state =
  (LVar (stateCounter state), state { stateCounter = stateCounter state + 1})

unify :: State -> LVar -> Type -> State
unify state lvar t  =
  let lvars = stateLVars state
      lvars' = (lvar, t) : lvars
  in state { stateLVars = lvars' }

infer :: State -> Context -> Term -> (Type, State)
infer state _ IntLiteral = (Int, state)
infer state _ BoolLiteral = (Bool, state)
infer state ctx (Conditional (cond, term1, term2)) =
  do
    let (ctvar, state) = newLVar state
    let (tvar1, state) = newLVar state
    let (tvar2, state) = newLVar state
    let state = unify state ctvar Bool
    let (ct, state) = infer state ctx cond
    let (t1, state) = infer state ctx term1
    let (t2, state) = infer state ctx term2
    let state = unify state ctvar ct
    let state = unify state tvar1 t1
    let state = unify state tvar2 t2
    let state = unify state tvar1 (TVar tvar2)
    (TVar tvar1, state)
infer state ctx (Variable name) =
  (Map.findWithDefault Undefined name ctx, state)
infer state ctx (Lambda (arg, body)) =
  do
    let (tvar1, state) = newLVar state
    let t1 = TVar tvar1
    let ctx' = Map.insert arg t1 ctx
    let (t2, state) = infer state ctx' body
    (Function (t1, t2), state)
infer state ctx (Application (fn, arg)) =
  do
    let (tvarf, state) = newLVar state
    let (tvar1, state) = newLVar state
    let (tvar2, state) = newLVar state
    let (ft, state) = infer state ctx fn
    let (t2, state) = infer state ctx arg
    let state = unify state tvarf ft
    let state = unify state tvarf (Function (TVar tvar1, TVar tvar2))
    let state = unify state tvar1 t2
    (TVar tvar2, state)
