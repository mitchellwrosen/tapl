{-# language LambdaCase #-}

module Eval where

import Term

import Bound (instantiate1)

isval :: Term a -> Bool
isval = \case
  Lam _ -> True
  _ -> False

eval1 :: Term a -> Maybe (Term a)
eval1 = \case
  App (Lam t1) t2 | isval t2 -> Just (instantiate1 t2 t1)
  App t1 t2 | isval t1 -> App t1 <$> eval1 t2
  App t1 t2 -> App <$> eval1 t1 <*> pure t2
  _ -> Nothing
