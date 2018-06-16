module Stlc.Token where

data Token
  = TokenAs
  | TokenBas
  | TokenBool
  | TokenCase
  | TokenCol
  | TokenCom
  | TokenDot
  | TokenFalse
  | TokenFix
  | TokenGal
  | TokenGar
  | TokenHepgar
  | TokenIn
  | TokenInt Int
  | TokenKel
  | TokenKer
  | TokenLet
  | TokenOf
  | TokenPal
  | TokenPar
  | TokenSem
  | TokenTis
  | TokenTisgar
  | TokenTrue
  | TokenUnit
  | TokenVar [Char]
  | TokenEOF
  deriving Show
