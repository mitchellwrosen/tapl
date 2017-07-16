{-# language DeriveFoldable     #-}
{-# language DeriveFunctor      #-}
{-# language DeriveTraversable  #-}
{-# language LambdaCase         #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell    #-}

module Term1 where

import Term
import Type (Type)

import Bound
import Control.Monad
import Data.Deriving
import Data.Functor.Classes

data Term1 a
  = Var1 a
  | Lam1 Type (Scope () Term1 a)
  | App1 (Term1 a) (Term1 a)
  | Bool1 Bool
  deriving (Foldable, Functor, Traversable)

instance Applicative Term1 where
  pure = return
  (<*>) = ap

instance Monad Term1 where
  return = Var1
  Var1 x >>= f = f x
  Lam1 ty x >>= f = Lam1 ty (x >>>= f)
  App1 x y >>= f = App1 (x >>= f) (y >>= f)
  Bool1 x >>= _ = Bool1 x

deriveShow1 ''Term1

deriving instance Show a => Show (Term1 a)

term1 :: Term -> Maybe (Term1 a)
term1 = closed . go
 where
  go :: Term -> Term1 String
  go = \case
    Var x -> Var1 x
    Lam x ty t -> Lam1 ty (abstract1 x (go t))
    App x y -> App1 (go x) (go y)
    Bool x -> Bool1 x
