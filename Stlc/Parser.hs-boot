module Stlc.Parser where

import Stlc.Lexer (Parser)
import Stlc.Token (Token)

parseError :: Token -> Parser a

lexer :: (Token -> Parser a) -> Parser a
