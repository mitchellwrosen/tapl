module Stlc.Token where

data Token
  = TokenAs
  | TokenBas
  | TokenBool
  | TokenCol
  | TokenCom
  | TokenDot
  | TokenFalse
  | TokenHepGar
  | TokenIn
  | TokenInt Int
  | TokenKel
  | TokenKer
  | TokenLet
  | TokenPal
  | TokenPar
  | TokenSem
  | TokenTis
  | TokenTrue
  | TokenUnit
  | TokenVar [Char]
  deriving Show
