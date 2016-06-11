{-# LANGUAGE LambdaCase #-}

module TypedExpr where

import qualified UntypedExpr as Untyped

import Control.Monad (guard)

-- T ::=      -- types:
--       Bool -- type of booleans
--       Nat  -- type of natural numbers

data Type
  = Bool
  | Nat
  deriving (Eq, Show)

typeof :: Untyped.U -> Maybe Type
typeof = \case
  -- T-True
  Untyped.Tru -> pure Bool

  -- T-False
  Untyped.Fls -> pure Bool

  -- T-If
  Untyped.Ifte e1 e2 e3 -> do
    Bool <- typeof e1
    t2   <- typeof e2
    t3   <- typeof e3
    guard (t2 == t3)
    pure t2

  -- T-Zero
  Untyped.Zero -> pure Nat

  -- T-Succ
  Untyped.Succ e -> do
    Nat <- typeof e
    pure Nat

  -- T-Pred
  Untyped.Pred e -> do
    Nat <- typeof e
    pure Nat

  -- T-IsZero
  Untyped.IsZero e -> do
    Nat <- typeof e
    pure Bool
