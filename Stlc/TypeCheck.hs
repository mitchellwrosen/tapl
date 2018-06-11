module Stlc.TypeCheck where

import Stlc.Term
import Stlc.Type

import Bound
import Control.Monad
import Data.Void

typeOf :: Term Void -> Maybe Type
typeOf =
  typeOf' . vacuous

typeOf' :: Term Type -> Maybe Type
typeOf' = \case
  TermVar x ->
    pure x
  TermLam y t -> do
    r <- typeOf' (instantiate1 (TermVar y) t)
    pure (TypeFun y r)
  TermApp t1 t2 -> do
    TypeFun y1 y2 <- typeOf' t1
    z <- typeOf' t2
    guard (y1 == z)
    pure y2
  TermLet t s -> do
    y <- typeOf' t
    typeOf' (instantiate1 (TermVar y) s)
  TermAs t y -> do
    z <- typeOf' t
    guard (y == z)
    pure y
  TermTuple ts ->
    TypeTuple <$> traverse typeOf' ts
  TermTupleIx t i -> do
    TypeTuple ys <- typeOf' t
    guard (i > 0 && i <= length ys)
    pure (ys !! (i-1))
  TermUnit ->
    pure TypeUnit
  TermTrue ->
    pure TypeBool
  TermFalse ->
    pure TypeBool
  TermIf t1 t2 t3 -> do
    TypeBool <- typeOf' t1
    y2 <- typeOf' t2
    y3 <- typeOf' t3
    guard (y2 == y3)
    pure y2
