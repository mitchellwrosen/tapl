{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module SimplyTypedLambdaCalc where

import Control.Monad  (guard)
import Data.Monoid
import Data.Set       (Set, (\\))
import Data.Text.Lazy (Text)

import qualified Data.Set as Set

-- t ::=                    -- terms:
--       x                  -- variable
--       \x:T.t             -- abstraction
--       t t                -- application
--
-- T ::=                    -- types:
--       T -> T             -- type of functions
--
-- Γ ::=                    -- contexts:
--       ∅                  -- empty context
--       Γ,x:T              -- term variable binding

data Term
  = Var Int
  | Lam Text Type Term
  | App Term Term
  deriving (Show)

data Type
  = Type :-> Type
  deriving (Eq, Show)
infixr :->

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
 where
  -- Safe version of (!!)
  ix :: Int -> [a] -> Maybe a
  ix _  []     = Nothing
  ix 0  (x:_)  = Just x
  ix !n (_:xs) = ix (n-1) xs
