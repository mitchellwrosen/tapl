{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

-- Simply typed lambda calculus extended with base types, unit, sequencing,
-- ascription, let bindings, tuples, and variants.

module ExtendedSimplyTypedLambdaCalc where

import Utils

import Control.Monad  (forM, guard)
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
--       t.i                -- tuple projection
--       {l=t,...}          -- record
--       t.l                -- record projection
--       <l=t> as T         -- tagging
--       case t of <l=x>=>t -- case
--       unit               -- constant unit
--
-- v ::=                    -- values:
--       \x:T.t             -- abstraction value
--       {v,...}            -- tuple value
--       {l=v,...}          -- record value
--       <l=v> as T         -- variant value
--       unit               -- unit value
--
--
-- T ::=                    -- types:
--       T -> T             -- type of functions
--       {T,...}            -- type of tuples
--       {l:T,...}          -- type of records
--       <l:T...>           -- type of variants
--       Unit               -- unit type
--       Base               -- base type (uninterpreted)
--
-- Γ ::=                    -- contexts:
--       ∅                  -- empty context
--       Γ,x:T              -- term variable binding

data Term
  = Var Int
  | Lam Hint Type Term
  | App Term Term
  | Asc Term Type
  | Seq Term Term
  | Let Hint Term Term
  | Tuple [Term]
  | PrjT Term Int
  | Record [(Label,Term)]
  | PrjR Term Label
  | Inj Label Term Type
  | Case Term [(Label,Hint,Term)]
  | Unit
  deriving (Show)

-- A hint about what to name the bound variable when pretty printing
type Hint = Text

type Label = Text

data Type
  = Type :-> Type
  | TyTuple [Type]
  | TyRecord [(Label,Type)]
  | TyVariant [(Label,Type)]
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
  Var n -> pure (c !! n)
  Lam _ ty1 t -> do
    ty2 <- typeof (ty1:c) t
    pure (ty1 :-> ty2)
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
  Tuple ts -> do
    tys <- mapM (typeof c) ts
    pure (TyTuple tys)
  PrjT t n -> do
    TyTuple ts <- typeof c t
    pure (ts !! n)
  Record ts0 -> do
    let (ls,ts) = unzip ts0
    tys <- mapM (typeof c) ts
    pure (TyRecord (zip ls tys))
  PrjR t l -> do
    TyRecord ts0 <- typeof c t
    lookup l ts0
  Inj l t (TyVariant tys) -> do
    ty  <- typeof c t
    ty' <- lookup l tys
    guard (ty == ty')
    pure (TyVariant tys)
  Case t bs -> do
    TyVariant tys0 <- typeof c t
    tys <- forM tys0 (\(li,tyi) -> do
      (_,ti) <- lookup2 li bs
      typeof (tyi:c) ti)
    alleq tys
  Unit -> pure TyUnit
  _ -> Nothing
 where
  alleq :: Eq a => [a] -> Maybe a
  alleq []     = Nothing
  alleq (a:as) = go as
   where
    go [] = Just a
    go (b:bs)
      | a == b    = go bs
      | otherwise = Nothing

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
    PrjT t _      -> go c t
    Record ts     -> foldMap (go c . snd) ts
    PrjR t _      -> go c t
    Inj _ t _     -> go c t
    Case t bs     -> go c t <> foldMap (\(_,_,ti) -> go (c+1) ti) bs
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
    PrjT t n      -> PrjT (go c t) n
    Record ts     -> Record (over (traverse._2) (go c) ts)
    PrjR t l      -> PrjR (go c t) l
    Inj l t ty    -> Inj l (go c t) ty
    Case t bs     -> Case (go c t) (over (traverse._3) (go (c+1)) bs)
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
  PrjT t n      -> PrjT (subst s0 t) n
  Record ts     -> Record (over (traverse._2) (subst s0) ts)
  PrjR t l      -> PrjR (subst s0 t) l
  Inj l t ty    -> Inj l (subst s0 t) ty
  Case t bs     -> Case (subst s0 t) (over (traverse._3) under bs)
  Unit          -> Unit
 where
  -- Substitute under a lambda: shift the inner term and apply the substitution
  -- on the shifted index instead.
  under :: Term -> Term
  under = subst (Subst (x+1) (shift 1 s))

-- Small-step call-by-value evaluation
eval :: Term -> Maybe Term
eval = \case
  Var _ -> Nothing

  Lam _ _ _ -> Nothing

  App (Lam _ _ t) v | isval v ->
    pure (beta v t)
  App v@(Lam _ _ _) t2 -> do
    t2' <- eval t2
    pure (App v t2')
  App t1 t2 -> do
    t1' <- eval t1
    pure (App t1' t2)

  Asc v _ | isval v ->
    pure v
  Asc t ty -> do
    t' <- eval t
    pure (Asc t' ty)

  Seq Unit t2 ->
    pure t2
  Seq t1 t2 -> do
    t1' <- eval t1
    pure (Seq t1' t2)

  Let _ v t | isval v ->
    pure (beta v t)
  Let n t1 t2 -> do
    t1' <- eval t1
    pure (Let n t1' t2)

  Tuple ts0 | (vs,t:ts) <- span isval ts0 -> do
    t' <- eval t
    pure (Tuple (vs ++ [t'] ++ ts))
  Tuple _ -> Nothing

  PrjT (Tuple ts) n | all isval ts ->
    pure (ts !! n)
  PrjT t n -> do
    t' <- eval t
    pure (PrjT t' n)

  PrjR (Record ts) l | all (isval . snd) ts ->
    lookup l ts
  PrjR t l -> do
    t' <- eval t
    pure (PrjR t' l)

  Record ts0 -> do
    let (ls,ts) = unzip ts0
    case span isval ts of
      (vs,t:tss) -> do
        t' <- eval t
        pure (Record (zip ls (vs ++ [t'] ++ tss)))
      _ -> Nothing

  -- FIXME: Do I need a "| not (isval t)" guard?
  Inj l t ty -> do
    t' <- eval t
    pure (Inj l t' ty)

  Case (Inj l v _) bs | isval v -> do
    (_,t) <- lookup2 l bs
    pure (beta v t)

  Case t bs -> do
    t' <- eval t
    pure (Case t' bs)

  Unit -> Nothing
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

--------------------------------------------------------------------------------

lookup2 :: Eq a => a -> [(a,b,c)] -> Maybe (b,c)
lookup2 _ [] = Nothing
lookup2 a ((a',b,d):xs)
  | a == a'   = Just (b,d)
  | otherwise = lookup2 a xs
