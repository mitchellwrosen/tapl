{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module SimplyTypedLambdaCalc where

import qualified UntypedLambdaCalc2 as Untyped

import Control.Monad (guard)

-- t ::=        -- terms:
--       x      -- variable
--       \x:T.t -- abstraction
--       t t    -- application
--
-- T ::=        -- types:
--       T -> T -- type of functions
--       Bool   -- type of booleans
--       Nat    -- type of natural numbers
--
-- Γ ::=        -- contexts:
--       ∅      -- empty context
--       Γ,x:T  -- term variable binding

data Term
  = Var Int
  | Lam Type Term
  | App Term Term
  deriving (Show)

data Value
  = VLam Type Term

data Type
  = Bool
  | Type :-> Type
  deriving (Eq, Show)
infixr :->

type Ctx = [Type]

typeof :: Ctx -> Term -> Maybe Type
typeof c = \case
  -- T-Var
  Var n -> ix n c
  -- T-Abs
  Lam ty1 t -> do
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

-- Typecheck a term, then compile it to the untyped lambda calculus.
compile :: Term -> Maybe Untyped.Term
compile t0 = do
  _ <- typeof [] t0
  pure (erase t0)
 where
  erase :: Term -> Untyped.Term
  erase = \case
    Var n     -> Untyped.Var n
    Lam _ t   -> Untyped.Lam (erase t)
    App t1 t2 -> Untyped.App (erase t1) (erase t2)
