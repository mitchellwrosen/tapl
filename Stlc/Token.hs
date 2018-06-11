module Stlc.Token where

data Token
  = TokenAs
  | TokenBas
  | TokenBool
  | TokenCol
  | TokenDot
  | TokenFalse
  | TokenHepGar
  | TokenPal
  | TokenPar
  | TokenSem
  | TokenTrue
  | TokenUnit
  | TokenVar [Char]
  deriving Show
