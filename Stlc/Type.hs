module Stlc.Type where

data Type
  = TypeFun Type Type
  | TypeBool
  | TypeUnit
  deriving (Eq, Show)
