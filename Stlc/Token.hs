module Stlc.Token where

data Token
  = TokenAs
  | TokenBas
  | TokenBool
  | TokenCol
  | TokenDot
  | TokenFalse
  | TokenHepGar
  | TokenIn
  | TokenLet
  | TokenPal
  | TokenPar
  | TokenSem
  | TokenTis
  | TokenTrue
  | TokenUnit
  | TokenVar [Char]
  deriving Show
