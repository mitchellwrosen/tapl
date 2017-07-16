module Lexer
  ( lex
  ) where

import GenLexer
import Token

import Prelude hiding (lex)

lex :: String -> [Token]
lex = alexScanTokens
