module Parser
  ( parseTerm
  , parseType
  ) where

import Lexer (lex)
import Term (Term)
import Type (Type)

import qualified GenParser

import Prelude hiding (lex)

parseTerm :: String -> Term
parseTerm = GenParser.parseTerm . lex

parseType :: String -> Type
parseType = GenParser.parseType . lex
