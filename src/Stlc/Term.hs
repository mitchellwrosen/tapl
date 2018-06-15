module Stlc.Term where

import Stlc.Label
import Stlc.Type

import Bound
import Control.Monad
import Data.Bifunctor
import Data.Foldable
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic1)

data Term a
  = TermVar a
  | TermLam Type (Scope () Term a)
  | TermApp (Term a) (Term a)
  | TermLet (Term a) (Scope () Term a)
  | TermAs (Term a) Type
  | TermTuple [Term a]
  | TermTupleIx (Term a) Int
  | TermRecord [(Label, Term a)]
  | TermRecordIx (Term a) Label
  | TermVariant Label (Term a) Type
  | TermCase (Term a) (NonEmpty (Label, Scope () Term a))
  | TermUnit
  | TermTrue
  | TermFalse
  | TermIf (Term a) (Term a) (Term a)
  deriving (Foldable, Functor, Generic1, Show, Traversable)

instance Applicative Term where
  pure = TermVar
  (<*>) = ap

instance Monad Term where
  return = pure
  TermVar x >>= f = f x
  TermLam y x >>= f = TermLam y (x >>>= f)
  TermApp x y >>= f = TermApp (x >>= f) (y >>= f)
  TermLet t s >>= f = TermLet (t >>= f) (s >>>= f)
  TermAs t y >>= f = TermAs (t >>= f) y
  TermTuple ts >>= f = TermTuple (map (>>= f) ts)
  TermTupleIx t i >>= f = TermTupleIx (t >>= f) i
  TermRecord ts >>= f = TermRecord ((map.second) (>>= f) ts)
  TermRecordIx t l >>= f = TermRecordIx (t >>= f) l
  TermVariant l t y >>= f = TermVariant l (t >>= f) y
  TermCase t us >>= f = TermCase (t >>= f) ((fmap.second) (>>>= f) us)
  TermUnit >>= _ = TermUnit
  TermTrue >>= _ = TermTrue
  TermFalse >>= _ = TermFalse
  TermIf t1 t2 t3 >>= f = TermIf (t1 >>= f) (t2 >>= f) (t3 >>= f)

instance Show1 Term where
  liftShowsPrec = liftShowsPrecDefault

pattern Value :: Term a -> Term a
pattern Value t <- (matchValue -> Just t)

matchValue :: Term a -> Maybe (Term a)
matchValue = \case
  t@TermLam{} -> Just t
  t@(TermTuple ts) -> t <$ traverse_ matchValue ts
  t@(TermRecord ts) -> t <$ traverse_ (matchValue . snd) ts
  t@(TermVariant _ v _) -> t <$ matchValue v
  TermUnit -> Just TermUnit
  TermTrue -> Just TermTrue
  TermFalse -> Just TermFalse

  TermVar{} -> Nothing
  TermApp{} -> Nothing
  TermLet{} -> Nothing
  TermAs{} -> Nothing
  TermTupleIx{} -> Nothing
  TermRecordIx{} -> Nothing
  TermCase{} -> Nothing
  TermIf{} -> Nothing

isValue :: Term a -> Bool
isValue = \case
  Value{} -> True
  _ -> False
