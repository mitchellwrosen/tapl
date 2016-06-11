{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Untyped lambda calculus, using string variable names and naive capture-
-- avoiding substitution.

module UntypedLambdaCalculus where

import Data.Monoid
import Data.Set    (Set, (\\))
import GHC.Exts    (IsString(..))
import Prelude     hiding (and, fst, not, or, snd, succ)

import qualified Data.Set                     as Set
import qualified Data.Text.Lazy               as LText
import qualified Text.PrettyPrint.Leijen.Text as PP

type LText = LText.Text


-- t ::=      -- terms:
--       x    -- variable
--       \x.t -- abstraction
--       t t  -- application
--
-- v ::=      -- values:
--       \x.t -- abstraction value

data Term
  = Var Var
  | Lam Var Term
  | App Term Term
  deriving (Show)
infixl 5 `App`

instance PP.Pretty Term where
  pretty = \case
    Var (V x) -> PP.text x
    Lam (V x) t -> PP.char 'λ' PP.<> PP.text x <> PP.char '.' PP.<+> PP.pretty t
    App t1 t2 ->
      case t1 of
        Lam _ _ ->
          case t2 of
            Var _ -> PP.parens (PP.pretty t1) PP.<+> PP.pretty t2
            _     -> PP.parens (PP.pretty t1) PP.<+> PP.parens (PP.pretty t2)
        _ ->
          case t2 of
            Var _ -> PP.pretty t1 PP.<+> PP.pretty t2
            _     -> PP.pretty t1 PP.<+> PP.parens (PP.pretty t2)


newtype Var = V LText
  deriving (Eq, Ord, Show, IsString)

data Subst = Var :-> Term


fvs :: Term -> Set Var
fvs (Var v)     = Set.singleton v
fvs (Lam x t)   = fvs t \\ Set.singleton x
fvs (App t1 t2) = fvs t1 <> fvs t2

-- @subst x s t@ substitutes all free occurrences of @x@ in @t@ with @s@.
--
-- [x -> s]t
--
-- For simplicity, we assume "user-made" variables cannot begin with a digit, so
-- so we can always reach for a fresh variable name.
subst :: Subst -> Term -> Term
subst = go (map (V . LText.pack . show) [0..])
 where
  go :: [Var] -> Subst -> Term -> Term
  go (n:ns) (x :-> s) = \case
    Var v
      | x == v    -> s
      | otherwise -> Var v
    -- Use 'go' rather than 'subst' when recursing, because we've used up one
    -- fresh variable name (n)
    Lam x' t -> Lam n (go ns (x :-> s) (alpha x' n t))
    App t1 t2 -> App (subst (x :-> s) t1) (subst (x :-> s) t2)

-- @alpha x y t@ renames all free occurrences of @x@ in @t@ to @y@. It is assumed
-- that @y@ is not free in @t@.
alpha :: Var -> Var -> Term -> Term
alpha x y = \case
  Var v
    | x == v    -> Var y
    | otherwise -> Var v
  Lam x' t
    | x == x'   -> Lam x' t
    | otherwise -> Lam x' (alpha x y t)
  App t1 t2 -> App (alpha x y t1) (alpha x y t2)

-- Small-step call-by-value evaluation
eval :: Term -> Term
eval = \case
  -- E-AppAbs
  App (Lam x t) v@(Lam _ _) -> subst (x :-> v) t
  -- E-App2
  App v@(Lam _ _) t2 -> let !t2' = eval t2
                        in App v t2'
  -- E-App1
  App t1 t2 -> let !t1' = eval t1
               in App t1' t2

---------------------------------------------------------------------------------
-- Some simple lambda calclus terms

-- tru = \t. \f. t
tru :: Term
tru = Lam "t" (Lam "f" (Var "t"))

-- fls = \t. \f. f
fls :: Term
fls = Lam "t" (Lam "f" (Var "f"))

-- test = \l. \m. \n. l m n
test :: Term
test = Lam "l" (Lam "m" (Lam "n" (Var "l" `App` Var "m" `App` Var "n")))

-- and = \b. \c. b c fls
and :: Term
and = Lam "b" (Lam "c" (Var "b" `App` Var "c" `App` fls))

-- or = \b. \c. b tru c
or :: Term
or = Lam "b" (Lam "c" (Var "b" `App` tru `App` Var "c"))

-- not = \b. b fls tru
not :: Term
not = Lam "b" (Var "b" `App` fls `App` tru)

-- pair = \f. \s. \b. b f s
pair :: Term
pair = Lam "f" (Lam "s" (Lam "b" (Var "b" `App` Var "f" `App` Var "s")))

-- fst = \p. p tru
fst :: Term
fst = Lam "p" (Var "p" `App` tru)

-- snd = \p. p fls
snd :: Term
snd = Lam "p" (Var "p" `App` fls)

-- c0 = \s. \z. z
c0 :: Term
c0 = Lam "s" (Lam "z" (Var "z"))

-- c1 = \s. \z. s z
c1 :: Term
c1 = Lam "s" (Lam "z" (Var "s" `App` Var "z"))

-- c2 = \s. \z. s (s z)
c2 :: Term
c2 = Lam "s" (Lam "z" (Var "s" `App` (Var "s" `App` Var "z")))

-- succ = \n. \s. \z. s (n s z)
succ :: Term
succ = Lam "n" (Lam "s" (Lam "z" (Var "s" `App` (Var "n" `App` Var "s" `App` Var "z"))))

-- plus :: \m. \n. \s. \z. m s (n s z)
plus :: Term
plus = Lam "m" (Lam "n" (Lam "s" (Lam "z" (Var "m" `App` Var "s" `App` (Var "n" `App` Var "s" `App` Var "z")))))

-- times :: \m. \n. m (plus n) c0
times :: Term
times = Lam "m" (Lam "n" (Var "m" `App` (plus `App` Var "n") `App` c0))

-- omega = (\x. x x) (\x. x x)
omega :: Term
omega = f `App` f
 where
  f :: Term
  f = Lam "x" (Var "x" `App` Var "x")

-- fix = \f. (\x. f (\y. x x y)) (\x. f (\y. x x y))
fix :: Term
fix = Lam "f" (g `App` g)
 where
  g :: Term
  g = Lam "x" (Var "f" `App` Lam "y" (Var "x" `App` Var "x" `App` Var "y"))
