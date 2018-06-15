module Stlc.Type where

import Stlc.Label

import Data.List.NonEmpty (NonEmpty)

data Type
  = TypeFun Type Type
  | TypeTuple [Type]
  | TypeRecord [(Label, Type)]
  | TypeVariant (NonEmpty (Label, Type))
  | TypeBool
  | TypeUnit
  deriving (Eq, Show)
