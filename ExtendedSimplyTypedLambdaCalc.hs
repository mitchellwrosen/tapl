{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

-- Simply typed lambda calculus extended with base types, unit, sequencing,
-- ascription, let bindings, and tuples.

module ExtendedSimplyTypedLambdaCalc where

import Control.Monad  (guard)
import Data.Monoid
import Data.Set       (Set)
import Data.Text.Lazy (Text)

import qualified Data.Set as Set

-- t ::=                    -- terms:
--       x                  -- variable
--       \x:T.t             -- abstraction
--       t t                -- application
--       t as T             -- ascription
--       t;t                -- sequence
--       let x=t in t       -- let binding
--       {t,...}            -- tuple
--       t.i                -- projection
--       unit               -- constant unit
--
-- v ::=                    -- values:
--       \x:T.t             -- abstraction value
--       {v,...}            -- tuple value
--       unit               -- unit value
--
--
-- T ::=                    -- types:
--       T -> T             -- type of functions
--       {T,...}            -- tuple type
--       Unit               -- unit type
--       Base               -- base type (uninterpreted)
--
-- Γ ::=                    -- contexts:
--       ∅                  -- empty context
--       Γ,x:T              -- term variable binding

data Term
  = Var Int
  | Lam Text Type Term
  | App Term Term
  | Asc Term Type
  | Seq Term Term
  | Let Text Term Term
  | Tuple [Term]
  | Prj Term Int
  | Unit
  deriving (Show)

data Type
  = Type :-> Type
  | TyTuple [Type]
  | TyUnit
  | TyBase Base
  deriving (Eq, Show)
infixr :->

data Base
  = Bool
  | Nat
  deriving (Eq, Show)

data Subst = Subst !Int Term

type Ctx = [Type]

typeof :: Ctx -> Term -> Maybe Type
typeof c = \case
  -- T-Var
  Var n -> pure (c !! n)
  -- T-Abs
  Lam _ ty1 t -> do
    ty2 <- typeof (ty1:c) t
    pure (ty1 :-> ty2)
  -- T-App
  App t1 t2 -> do
    ty1 :-> ty2 <- typeof c t1
    ty1'        <- typeof c t2
    guard (ty1 == ty1')
    pure ty2
  Asc t ty -> do
    ty' <- typeof c t
    guard (ty == ty')
    pure ty
  Seq t1 t2 -> do
    TyUnit <- typeof c t1
    typeof c t2
  Let _ t1 t2 -> do
    ty1 <- typeof c t1
    typeof (ty1:c) t2
  -- T-Tuple
  Tuple ts -> do
    tys <- mapM (typeof c) ts
    pure (TyTuple tys)
  -- T-Proj
  Prj t n -> do
    TyTuple ts <- typeof c t
    pure (ts !! n)
  Unit -> pure TyUnit

-- @fvs t@ finds all free variables in @t@.
fvs :: Term -> Set Int
fvs = go 0
 where
  go :: Int -> Term -> Set Int
  go !c = \case
    Var v
      | v >= c    -> Set.singleton v
      | otherwise -> mempty
    Lam _ _ t     -> go (c+1) t
    App t1 t2     -> go c t1 <> go c t2
    Asc t _       -> go c t
    Seq t1 t2     -> go c t1 <> go c t2
    Let _ t1 t2   -> go c t1 <> go (c+1) t2
    Tuple ts      -> foldMap (go c) ts
    Prj t _       -> go c t
    Unit          -> mempty

-- @shift d t@ shifts all free variables in @t@ by @d@.
shift :: Int -> Term -> Term
shift d = go 0
 where
  go :: Int -> Term -> Term
  go !c = \case
    Var k
      | k < c     -> Var k
      | otherwise -> Var (k+d)
    Lam n ty t    -> Lam n ty (go (c+1) t)
    App t1 t2     -> App (go c t1) (go c t2)
    Asc t ty      -> Asc (go c t) ty
    Seq t1 t2     -> Seq (go c t1) (go c t2)
    Let n t1 t2   -> Let n (go c t1) (go (c+1) t2)
    Tuple ts      -> Tuple (map (go c) ts)
    Prj t n       -> Prj (go c t) n
    Unit          -> Unit

-- @subst x s t@ substitutes all free occurrences of @x@ in @t@ with @s@.
--
-- [x -> s]t
subst :: Subst -> Term -> Term
subst s0@(Subst x s) = \case
  Var v
    | x == v    -> s
    | otherwise -> Var v
  Lam n ty t    -> Lam n ty (under t)
  App t1 t2     -> App (subst s0 t1) (subst s0 t2)
  Asc t ty      -> Asc (subst s0 t) ty
  Seq t1 t2     -> Seq (subst s0 t1) (subst s0 t2)
  Let n t1 t2   -> Let n (subst s0 t1) (under t2)
  Tuple ts      -> Tuple (map (subst s0) ts)
  Prj t n       -> Prj (subst s0 t) n
  Unit          -> Unit
 where
  -- Shift under a lambda
  under :: Term -> Term
  under = subst (Subst (x+1) (shift 1 s))

-- Small-step call-by-value evaluation
eval :: Term -> Maybe Term
eval = \case
  -- E-AppAbs
  App (Lam _ _ t) v | isval v ->
    pure (beta v t)
  -- E-App2
  App v@(Lam _ _ _) t2 -> do
    t2' <- eval t2
    pure (App v t2')
  -- E-App1
  App t1 t2 -> do
    t1' <- eval t1
    pure (App t1' t2)
  -- E-Ascribe
  Asc v _ | isval v ->
    pure v
  -- E-Ascribe1
  Asc t ty -> do
    t' <- eval t
    pure (Asc t' ty)
  -- E-SeqNext
  Seq Unit t2 ->
    pure t2
  Seq t1 t2 -> do
    t1' <- eval t1
    pure (Seq t1' t2)
  -- E-LetV
  Let _ v t | isval v ->
    pure (beta v t)
  -- E-Let
  Let n t1 t2 -> do
    t1' <- eval t1
    pure (Let n t1' t2)
  -- E-ProjTuple
  Prj (Tuple ts) n | all isval ts ->
    pure (ts !! n)
  -- E-Proj
  Prj t n -> do
    t' <- eval t
    pure (Prj t' n)
  -- E-Tuple
  Tuple ts0 | (vs,t:ts) <- span isval ts0 -> do
    t' <- eval t
    pure (Tuple (vs ++ [t'] ++ ts))
  _ -> Nothing
 where
  isval :: Term -> Bool
  isval = \case
    Lam _ _ _  -> True
    Tuple ts   -> all isval ts
    Unit       -> True
    _          -> False

  -- @beta s t@ subtitutes s for variable 0 in t.
  beta :: Term -> Term -> Term
  beta s t = shift (-1) (subst (Subst 0 (shift 1 s)) t)

-- Big-step call-by-value evaluation
eval' :: Term -> Term
eval' t0 = maybe t0 eval' (eval t0)
