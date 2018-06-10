module Stlc.Parser where

import Stlc.Happy
import Stlc.Lexer
import Stlc.Term

import Data.ByteString (ByteString)

parseTerm :: ByteString -> Term [Char]
parseTerm =
  Stlc.Happy.parse . Stlc.Lexer.lex
