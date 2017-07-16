{-# language LambdaCase #-}

module Check where

import Term1
import Type

import Bound
import Control.Monad
import Data.Void

check :: Term1 Void -> Maybe Type
check = go . vacuous
 where
  go :: Term1 Type -> Maybe Type
  go = \case
    Var1 ty -> Just ty
    Lam1 t1 x -> do
      t2 <- go (instantiate1 (Var1 t1) x)
      pure (TyArrow t1 t2)
    App1 x y -> do
      TyArrow t1 t2 <- go x
      t3 <- go y
      guard (t1 == t3)
      pure t2
    Bool1 _ -> Just TyBool
