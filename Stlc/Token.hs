module Stlc.Token where

data Token
  = TokenBas
  | TokenCol
  | TokenDot
  | TokenHepGar
  | TokenPal
  | TokenPar
  | TokenSem
  | TokenVar [Char]
  | TokenUnit
  | TokenBool
  | TokenTrue
  | TokenFalse
  deriving Show
