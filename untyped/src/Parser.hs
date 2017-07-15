module Parser
  ( parse
  ) where

import Term (Term)
import Lexer (lex)

import qualified GenParser

import Prelude hiding (lex)

parse :: String -> Term String
parse = GenParser.parse . lex
