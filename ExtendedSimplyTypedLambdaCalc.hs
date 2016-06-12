{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

-- Simply typed lambda calculus extended with base types, unit, sequencing,
-- ascription, let bindings, and pairs.

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
--       {t,t}              -- pair
--       t.1                -- first projection
--       t.2                -- second projection
--       unit               -- constant unit
--
-- v ::=                    -- values:
--       \x:T.t             -- abstraction value
--       {v,v}              -- pair value
--       unit               -- unit value
--
--
-- T ::=                    -- types:
--       T -> T             -- type of functions
--       T x T              -- product type
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
  | Pair Term Term
  | Fst Term
  | Snd Term
  | Unit
  deriving (Show)

data Type
  = Type :-> Type
  | Type :*  Type
  | TyUnit
  | TyBase Base
  deriving (Eq, Show)
infixr :->
infixr :*

data Base
  = Bool
  | Nat
  deriving (Eq, Show)

data Subst = Subst !Int Term

type Ctx = [Type]

typeof :: Ctx -> Term -> Maybe Type
typeof c = \case
  -- T-Var
  Var n -> ix n c
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
  -- T-Pair
  Pair t1 t2 -> do
    ty1 <- typeof c t1
    ty2 <- typeof c t2
    pure (ty1 :* ty2)
  -- T-Proj1
  Fst t -> do
    ty :* _ <- typeof c t
    pure ty
  -- T-Proj2
  Snd t -> do
    _ :* ty <- typeof c t
    pure ty
  Unit -> pure TyUnit
 where
  -- Safe version of (!!)
  ix :: Int -> [a] -> Maybe a
  ix _  []     = Nothing
  ix 0  (x:_)  = Just x
  ix !n (_:xs) = ix (n-1) xs

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
    Pair t1 t2    -> go c t1 <> go c t2
    Fst t         -> go c t
    Snd t         -> go c t
    Unit          -> mempty

-- @shift d t@ shifts all free variables in @t@ by @d@.
shift :: Int -> Term -> Term
shift d = go 0
 where
  go :: Int -> Term -> Term
  go !c = \case
    Var k
      | k < c     -> Var k
      | otherwise ->
          let
            !k' = k + d
          in
            Var k'
    Lam n ty t ->
      let
        !t' = go (c+1) t
      in
        Lam n ty t'
    App t1 t2 ->
      let
        !t1' = go c t1
        !t2' = go c t2
      in
        App t1' t2'
    Asc t ty ->
      let
        !t' = go c t
      in
        Asc t' ty
    Seq t1 t2 ->
      let
        !t1' = go c t1
        !t2' = go c t2
      in
        Seq t1' t2'
    Let n t1 t2 ->
      let
        !t1' = go c t1
        !t2' = go (c+1) t2
      in
        Let n t1' t2'
    Pair t1 t2 ->
      let
        !t1' = go c t1
        !t2' = go c t2
      in
        Pair t1' t2'
    Fst t ->
      let
        !t' = go c t
      in
        Fst t'
    Snd t ->
      let
        !t' = go c t
      in
        Snd t'
    Unit -> Unit


-- @subst x s t@ substitutes all free occurrences of @x@ in @t@ with @s@.
--
-- [x -> s]t
subst :: Subst -> Term -> Term
subst s0@(Subst x s) = \case
  Var v
    | x == v    -> s
    | otherwise -> Var v
  Lam n ty t ->
    let
      !s' = shift 1 s
      !t' = subst (Subst (x+1) s') t
    in
      Lam n ty t'
  App t1 t2 ->
    let
      !t1' = subst s0 t1
      !t2' = subst s0 t2
    in
      App t1' t2'
  Asc t ty ->
    let
      !t' = subst s0 t
    in
      Asc t' ty
  Seq t1 t2 ->
    let
      !t1' = subst s0 t1
      !t2' = subst s0 t2
    in
      Seq t1' t2'
  Let n t1 t2 ->
    let
      !t1' = subst s0 t1
      !s'  = shift 1 s
      !t2' = subst (Subst (x+1) s') t2
    in
      Let n t1' t2'
  Pair t1 t2 ->
    let
      !t1' = subst s0 t1
      !t2' = subst s0 t2
    in
      Pair t1' t2'
  Fst t ->
    let
      !t' = subst s0 t
    in
      Fst t'
  Snd t ->
    let
      !t' = subst s0 t
    in
      Snd t'
  Unit -> Unit


-- Small-step call-by-value evaluation
eval :: Term -> Maybe Term
eval = \case
  -- E-AppAbs
  App (Lam _ _ t) v | isval v ->
    pure (beta v t)
  -- E-App2
  App v@(Lam _ _ _) t2 -> do
    !t2' <- eval t2
    pure (App v t2')
  -- E-App1
  App t1 t2 -> do
    !t1' <- eval t1
    pure (App t1' t2)
  -- E-Ascribe
  Asc v _ | isval v ->
    pure v
  -- E-Ascribe1
  Asc t ty -> do
    !t' <- eval t
    pure (Asc t' ty)
  -- E-SeqNext
  Seq Unit t2 ->
    pure t2
  Seq t1 t2 -> do
    !t1' <- eval t1
    pure (Seq t1' t2)
  -- E-LetV
  Let _ v t | isval v ->
    pure (beta v t)
  -- E-Let
  Let n t1 t2 -> do
    !t1' <- eval t1
    pure (Let n t1' t2)
  -- E-PairBeta1
  Fst v@(Pair v1 _) | isval v ->
    pure v1
  -- E-PairBeta2
  Snd v@(Pair _ v2) | isval v ->
    pure v2
  -- E-Proj1
  Fst t -> do
    t' <- eval t
    pure (Fst t')
  -- E-Proj2
  Snd t -> do
    t' <- eval t
    pure (Snd t')
  Pair t1 t2
    -- E-Pair2
    | isval t1 -> do
        t2' <- eval t2
        pure (Pair t1 t2')
    -- E-Pair1
    | otherwise -> do
        t1' <- eval t1
        pure (Pair t1' t2)
  _ -> Nothing
 where
  isval :: Term -> Bool
  isval = \case
    Lam _ _ _  -> True
    Pair t1 t2 -> isval t1 && isval t2
    Unit       -> True
    _          -> False

  -- @beta s t@ subtitutes s for variable 0 in t.
  beta :: Term -> Term -> Term
  beta s t = shift (-1) (subst (Subst 0 (shift 1 s)) t)

-- Big-step call-by-value evaluation
eval' :: Term -> Term
eval' t0 = maybe t0 eval' (eval t0)
