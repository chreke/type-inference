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
solve (TVar v) t subst =
  let res1 = Map.lookup v subst
      res2 =
        case t of
          (TVar v') -> Map.lookup v' subst
          _ -> Nothing
   in case (res1, res2) of
        (Just t', _) -> solve t' t subst
        (_, Just t') -> solve (TVar v) t' subst
        -- TODO: Add the occurs check
        _ -> Right $ Map.insert v t subst
solve t (TVar v) subst = solve (TVar v) t subst
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
        _
          | occursCheck v t subst ->
              Left (printf "Self-reference: %s %s" (show v) (show t))
        _ -> Right $ Map.insert v t subst

-- If v is bound, unify with the bound value
-- If x is a bound variable, unify with the bound value
-- Otherwise, add a variable mapping
-- If   either variable is bound, get the bound variable
--
-- solveVar v1 (TVar v2) subst =
-- case Map.lookup v2 subst of
--   Just t ->
-- solveVar v t subst =
--   case Map.lookup v subst of
--     Just t' -> solve t' t subst
--     Nothing ->
--       case t of
--         TVar v' ->
--           case Map.lookup v' subst of
--             Just t' -> solve (TVar v) t' subst
--             Nothing -> Just $ Map.insert v t subst
--         _ -> Just $ Map.insert v t subst
-- TODO: Add occurs check

newVar :: State Int LVar
newVar = do
  s <- get
  let next = s + 1
  put s
  return (LVar next)

data LTerm
  = LIntLiteral
  | LBoolLiteral
  | LConditional ((LVar, LTerm), (LVar, LTerm), (LVar, LTerm))
  | LLambda (LVar, (LVar, LTerm))
  | LVariable LVar
  | LApplication ((LVar, LTerm), (LVar, LTerm))
  | LUndefined
  deriving (Eq, Show)

type Symbols = Map String LVar

label :: Symbols -> Term -> State Int LTerm
label _ IntLiteral = pure LIntLiteral
label _ BoolLiteral = pure LBoolLiteral
label symbols (Conditional (t1, t2, t3)) =
  do
    v1 <- newVar
    v2 <- newVar
    v3 <- newVar
    t1' <- label symbols t1
    t2' <- label symbols t2
    t3' <- label symbols t3
    return $ LConditional ((v1, t1'), (v2, t2'), (v3, t3'))
label symbols (Lambda (x, t)) =
  do
    var1 <- newVar
    var2 <- newVar
    let symbols' = Map.insert x var1 symbols
    t' <- label symbols' t
    return $ LLambda (var1, (var2, t'))
label symbols (Variable name) =
  case Map.lookup name symbols of
    Just var -> pure $ LVariable var
    Nothing -> pure LUndefined
label symbols (Application (t1, t2)) =
  do
    t1' <- label symbols t1
    t2' <- label symbols t2
    v1 <- newVar
    v2 <- newVar
    return $ LApplication ((v1, t1'), (v2, t2'))

infer' :: LVar -> LTerm -> [(LVar, Type)]
infer' var LIntLiteral =
  [(var, Int)]
infer' var LBoolLiteral =
  [(var, Bool)]
infer' var (LConditional ((v1, t1), (v2, t2), (v3, t3))) =
  [(v1, Bool), (v2, TVar v3), (var, TVar v2)]
    ++ infer' v1 t1
    ++ infer' v2 t2
    ++ infer' v3 t3
infer' var (LLambda (v1, (v2, t))) =
  infer' v2 t
    ++ [(var, Function (TVar v1, TVar v2))]
infer' var (LVariable v1) =
  [(var, TVar v1)]
infer' var (LApplication ((v1, t1), (v2, t2))) =
  [(v1, Function (TVar v2, TVar var))]
    ++ infer' v1 t1
    ++ infer' v2 t2
infer' var LUndefined =
  [(var, Undefined)]

unify' :: Type -> Type -> Map LVar Type -> Either String (Map LVar Type)
unify' t1 t2 subst | t1 == t2 = Right subst
unify' Undefined _ _ = Left "Undefined"
unify' _ Undefined _ = Left "Undefined"
unify' (TVar v) t subst =
  let res1 = Map.lookup v subst
      res2 =
        case t of
          (TVar v') -> Map.lookup v' subst
          _ -> Nothing
   in case (res1, res2) of
        (Just t', _) -> unify' t' t subst
        (_, Just t') -> unify' (TVar v) t' subst
        -- TODO: Add the occurs check
        _ -> Right $ Map.insert v t subst
unify' t (TVar v) subst = unify' (TVar v) t subst
unify' (Function (in1, out1)) (Function (in2, out2)) subst =
  unify' in1 in2 subst >>= unify' out1 out2
unify' t1 t2 _subst = Left (printf "Could not unify %s with %s" (show t1) (show t2))

-- Map.lookup v >>=
-- Map.lookup v of
--   Just t' -> solve t' t subst
-- TODO: Add occurs check

check :: Term -> (Type, AppState)
check term =
  let ctx = Map.empty
      initalState =
        AppState
          { appConstraints = [],
            appCounter = 0
          }
   in runState (infer ctx term) initalState
