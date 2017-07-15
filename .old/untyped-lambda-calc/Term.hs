module Term where

import Data.Monoid
import Data.Set       (Set, (\\))
import Data.Text.Lazy (Text)

import qualified Data.Set                     as Set

-- t ::=      -- terms:
--       x    -- variable
--       \x.t -- abstraction
--       t t  -- application
--
-- v ::=      -- values:
--       \x.t -- abstraction value
data Term
  = Var Var
  | Lam Hint Term
  | App Term Term
  deriving (Show)
infixl 5 `App`

-- | Variables are represented by De Bruijn indexes
type Var = Int

-- | A hint to the pretty-printer about what to name a bound variable.
type Hint = Text


-- Is this term a value?
isval :: Term -> Bool
isval = \case
  Lam _ _ -> True
  _       -> False

-- @fvs t@ finds all free variables in @t@.
fvs :: Term -> Set Var
fvs = go 0
 where
  go :: Int -> Term -> Set Var
  go c = \case
    Var v
      | v >= c    -> Set.singleton v
      | otherwise -> mempty
    Lam _ t -> go (c+1) t
    App t1 t2 -> go c t1 <> go c t2

-- @shift d t@ shifts all free variables in @t@ by @d@.
shift :: Int -> Term -> Term
shift d = go 0
 where
  go :: Int -> Term -> Term
  go c = \case
    Var k
      | k < c     -> Var k
      | otherwise -> Var (k + d)
    Lam n t -> Lam n (go (c+1) t)
    App t1 t2 -> App (go c t1) (go c t2)
