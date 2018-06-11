module Stlc.Type where

import Stlc.Label

data Type
  = TypeFun Type Type
  | TypeTuple [Type]
  | TypeRecord [(Label, Type)]
  | TypeBool
  | TypeUnit
  deriving (Eq, Show)
