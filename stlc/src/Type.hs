module Type where

data Type
  = TyBool
  | TyArrow Type Type
  deriving (Eq, Show)
