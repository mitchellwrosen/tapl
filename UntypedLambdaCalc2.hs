{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module UntypedLambdaCalc2 where

-- | Untyped lambda calculus, using De Bruijn indices.

import Data.Monoid
import Data.Set    (Set, (\\))
import Prelude     hiding (and, fst, not, or, snd, succ)

import qualified Data.Set                     as Set
import qualified Text.PrettyPrint.Leijen.Text as PP

-- t ::=      -- terms:
--       x    -- variable
--       \x.t -- abstraction
--       t t  -- application
--
-- v ::=      -- values:
--       \x.t -- abstraction value


data Term
  = Var Int
  | Lam Term
  | App Term Term
  deriving (Show)
infixl 5 `App`

instance PP.Pretty Term where
  pretty = \case
    Var n -> PP.int n
    Lam t ->
      case t of
        Lam _ -> PP.char 'λ' PP.<> PP.pretty t
        _     -> PP.char 'λ' PP.<+> PP.pretty t
    App t1 t2 ->
      case t1 of
        Lam _ ->
          case t2 of
            Var _ -> PP.parens (PP.pretty t1) PP.<+> PP.pretty t2
            _     -> PP.parens (PP.pretty t1) PP.<+> PP.parens (PP.pretty t2)
        _     ->
          case t2 of
            Var _ -> PP.pretty t1 PP.<+> PP.pretty t2
            _     -> PP.pretty t1 PP.<+> PP.parens (PP.pretty t2)


data Subst = !Int :-> Term
infixl 5 :->

-- @fvs t@ finds all free variables in @t@.
fvs :: Term -> Set Int
fvs = go 0
 where
  go :: Int -> Term -> Set Int
  go !c = \case
    Var v
      | v >= c    -> Set.singleton v
      | otherwise -> mempty
    Lam t -> go (c+1) t
    App t1 t2 -> go c t1 <> go c t2

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
    Lam t ->
      let
        !t' = go (c+1) t
      in
        Lam t'
    App t1 t2 ->
      let
        !t1' = go c t1
        !t2' = go c t2
      in
        App t1' t2'

-- @subst x s t@ substitutes all free occurrences of @x@ in @t@ with @s@.
--
-- [x -> s]t
subst :: Subst -> Term -> Term
subst (x :-> s) = \case
  Var v
    | x == v    -> s
    | otherwise -> Var v
  Lam t ->
    let
      !s' = shift 1 s
      !t' = subst (x+1 :-> s') t
    in
      Lam t'
  App t1 t2 ->
    let
      !t1' = subst (x :-> s) t1
      !t2' = subst (x :-> s) t2
    in
      App t1' t2'

-- Small-step call-by-value evaluation
eval :: Term -> Maybe Term
eval = \case
  -- E-AppAbs
  App (Lam t) v@(Lam _) -> pure (shift (-1) (subst (0 :-> shift 1 v) t))
  -- E-App2
  App v@(Lam _) t2 -> do
    !t2' <- eval t2
    pure (App v t2')
  -- E-App1
  App t1 t2 -> do
    !t1' <- eval t1
    pure (App t1' t2)
  _ -> Nothing

-- Big-step call-by-value evaluation
eval' :: Term -> Term
eval' t0 = maybe t0 eval' (eval t0)


---------------------------------------------------------------------------------
-- Some simple lambda calclus terms

-- tru = \\1
tru :: Term
tru = Lam (Lam (Var 1))

-- fls = \\0
fls :: Term
fls = Lam (Lam (Var 0))

-- test = \\\2 1 0
test :: Term
test = Lam (Lam (Lam (Var 2 `App` Var 1 `App` Var 0)))

-- and = \\1 0 fls
and :: Term
and = Lam (Lam (Var 1 `App` Var 0 `App` fls))

-- or = \\1 tru 0
or :: Term
or = Lam (Lam (Var 1 `App` tru `App` Var 0))

-- not = \0 fls tru
not :: Term
not = Lam (Var 0 `App` fls `App` tru)

-- pair = \\\0 2 1
pair :: Term
pair = Lam (Lam (Lam (Var 0 `App` Var 2 `App` Var 1)))

-- fst = \0 tru
fst :: Term
fst = Lam (Var 0 `App` tru)

-- snd = \0 fls
snd :: Term
snd = Lam (Var 0 `App` fls)

-- c0 = \\0
c0 :: Term
c0 = Lam (Lam (Var 0))

-- (\a. \b. \c. b (a b c)) (\s. \z. z)
-- \b. \c. b (c0 b c)
-- \b. \c. b ((\s. \z. z) b c)
--
-- \ \ 1 ((\\0) 1 0

-- c1 = \\1 0
c1 :: Term
c1 = Lam (Lam (Var 1 `App` Var 0))

-- c2 = \\1 (1 0)
c2 :: Term
c2 = Lam (Lam (Var 1 `App` (Var 1 `App` Var 0)))

-- succ = \\\1 (2 1 0)
succ :: Term
succ = Lam (Lam (Lam (Var 1 `App` (Var 2 `App` Var 1 `App` Var 0))))

-- plus :: \\\\3 1 (2 1 0)
plus :: Term
plus = Lam (Lam (Lam (Lam (Var 3 `App` Var 1 `App` (Var 2 `App` Var 1 `App` Var 0)))))

-- times :: \\1 (plus 0) c0
times :: Term
times = Lam (Lam (Var 1 `App` (plus `App` Var 0) `App` c0))

-- omega = (\0 0) (\0 0)
omega :: Term
omega = f `App` f
 where
  f :: Term
  f = Lam (Var 0 `App` Var 0)

-- fix = \(\1 (\1 1 0)) (\1 (\1 1 0))
fix :: Term
fix = Lam (g `App` g)
 where
  g :: Term
  g = Lam (Var 1 `App` Lam (Var 1 `App` Var 1 `App` Var 0))
