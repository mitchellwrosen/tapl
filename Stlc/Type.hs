module Stlc.Type where

data Type
  = TypeFun Type Type
  | TypeTuple [Type]
  | TypeBool
  | TypeUnit
  deriving (Eq, Show)
