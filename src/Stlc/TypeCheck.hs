module Stlc.TypeCheck where

import Stlc.Term
import Stlc.Type

import Bound
import Control.Monad
import Data.Foldable (for_, toList)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty(..))
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
  TermFix t -> do
    TypeFun u v <- typeOf' t
    guard (u == v)
    pure u
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
  TermRecord ts -> do
    ys <- traverse (typeOf' . snd) ts
    guard (length (nub (map fst ts)) == length ts)
    pure (TypeRecord (zipWith ((,) . fst) ts ys))
  TermRecordIx t l -> do
    TypeRecord ys <- typeOf' t
    lookup l ys
  TermVariant l t y -> do
    x <- typeOf' t
    TypeVariant zs <- pure y
    z <- lookup l (toList zs)
    guard (x == z)
    pure y
  TermCase t ((l1, u1) :| us) -> do
    TypeVariant (toList -> ys) <- typeOf' t
    y1 <- lookup l1 ys
    z1 <- typeOf' (instantiate1 (TermVar y1) u1)
    for_ us $ \(l, u) -> do
      y <- lookup l ys
      z <- typeOf' (instantiate1 (TermVar y) u)
      guard (z == z1)
    for_ ys $ \(l, _) ->
      guard (l `elem` l1 : map fst us)
    pure z1
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
