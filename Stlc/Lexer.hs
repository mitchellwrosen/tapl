module Stlc.Lexer where

import Stlc.Alex
import Stlc.Token

import Data.ByteString (ByteString)
import Prelude hiding (lex)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Latin1

lex :: ByteString -> [Token]
lex s =
  case alexScan s 0 of
    AlexEOF ->
      []
    AlexError s' ->
      error ("Lexical error; remaining input: " ++ Latin1.unpack s')
    AlexSkip s' _ ->
      lex s'
    AlexToken s' n tk ->
      tk (Latin1.unpack (ByteString.take n s)) : lex s'
