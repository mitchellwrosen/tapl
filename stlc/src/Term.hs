module Term where

import Type (Type)

import Control.Monad

data Term
  = Var String
  | Lam String Type Term
  | App Term Term
  | Bool Bool
  deriving (Eq, Show)
