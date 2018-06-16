module Stlc.Parser where

import Stlc.Alex
import Stlc.Happy
import Stlc.Lexer
import Stlc.Term
import Stlc.Token

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.ByteString (ByteString)
import Prelude hiding (lex)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Latin1

parseError :: Token -> Parser a
parseError t = do
  (bytes, code) <- get
  throwError (ParseError bytes code t)

parseTerm :: ByteString -> Either E (Term [Char])
parseTerm bytes =
  evalStateT parser (bytes, 0)

lexer :: (Token -> Parser a) -> Parser a
lexer = (lex >>=)

lex :: Parser Token
lex = do
  (bytes, code) <- get
  case alexScan bytes code of
    AlexEOF ->
      pure TokenEOF
    AlexError bytes' ->
      throwError (LexError bytes')
    AlexSkip bytes' _ -> do
      put (bytes', code)
      lex
    AlexToken bytes' n f -> do
      put (bytes', code)
      f (Latin1.unpack (ByteString.take n bytes))
