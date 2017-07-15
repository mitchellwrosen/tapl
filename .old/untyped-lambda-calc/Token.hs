module Token where

import Data.Text (Text)

data Token
  = TkVar Text
  | TkLam
  | TkSpace
  | TkDot
  | TkLParen
  | TkRParen
  deriving Show
