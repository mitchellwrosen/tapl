module Stlc.Term where

import Stlc.Type

import Bound
import Control.Monad
import Control.Monad.Trans
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import GHC.Generics (Generic1)

data Term a
  = TermVar a
  | TermLam Type (Scope () Term a)
  | TermApp (Term a) (Term a)
  | TermAs (Term a) Type
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
  TermLam y x >>= f = TermLam y (x >>= lift . f)
  TermApp x y >>= f = TermApp (x >>= f) (y >>= f)
  TermAs t y >>= f = TermAs (t >>= f) y
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
  TermUnit -> Just TermUnit
  TermTrue -> Just TermTrue
  TermFalse -> Just TermFalse

  TermVar{} -> Nothing
  TermApp{} -> Nothing
  TermAs{} -> Nothing
  TermIf{} -> Nothing
