{-# language DeriveFunctor #-}

module Term where

import Bound
import Control.Monad

data Term a
  = Var a
  | Lam (Scope () Term a)
  | App (Term a) (Term a)
  deriving (Functor)

instance Applicative Term where
  pure = return
  (<*>) = ap

instance Monad Term where
  return = Var
  Var x >>= f = f x
  Lam x >>= f = Lam (x >>>= f)
  App x y >>= f = App (x >>= f) (y >>= f)
