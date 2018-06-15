module Stlc.Lexer where

import Stlc.Token

import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Word (Word8)

import qualified Data.ByteString as ByteString

type AlexInput
  = ByteString

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte =
  ByteString.uncons

type Parser a
  = StateT S (Either E) a

type S
  = (ByteString, Int)

data E
  = LexError ByteString
  | ParseError ByteString Int Token
  deriving Show

tkAs     = tk  TokenAs
tkBas    = tk  TokenBas
tkBool   = tk  TokenBool
tkCase   = tk  TokenCase
tkCol    = tk  TokenCol
tkCom    = tk  TokenCom
tkDot    = tk  TokenDot
tkFalse  = tk  TokenFalse
tkGal    = tk  TokenGal
tkGar    = tk  TokenGar
tkHepgar = tk  TokenHepgar
tkIn     = tk  TokenIn
tkInt    = tk' (TokenInt . read)
tkKel    = tk  TokenKel
tkKer    = tk  TokenKer
tkLet    = tk  TokenLet
tkOf     = tk  TokenOf
tkPal    = tk  TokenPal
tkPar    = tk  TokenPar
tkSem    = tk  TokenSem
tkTis    = tk  TokenTis
tkTisgar = tk  TokenTisgar
tkTrue   = tk  TokenTrue
tkUnit   = tk  TokenUnit
tkVar    = tk' TokenVar

tk :: Token -> [Char] -> Parser Token
tk t _ =
  pure t

tk' :: ([Char] -> Token) -> [Char] -> Parser Token
tk' =
  fmap pure
