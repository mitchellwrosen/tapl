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
  | ParseError Token

tkTis :: [Char] -> Parser Token
tkTis _ =
  pure TokenTis

tkAs :: [Char] -> Parser Token
tkAs _ =
  pure TokenAs

tkBool :: [Char] -> Parser Token
tkBool _ =
  pure TokenBool

tkFalse :: [Char] -> Parser Token
tkFalse _ =
  pure TokenFalse

tkIn :: [Char] -> Parser Token
tkIn _ =
  pure TokenIn

tkLet :: [Char] -> Parser Token
tkLet _ =
  pure TokenLet

tkTrue :: [Char] -> Parser Token
tkTrue _ =
  pure TokenTrue

tkUnit :: [Char] -> Parser Token
tkUnit _ =
  pure TokenUnit

tkVar :: [Char] -> Parser Token
tkVar s =
  pure (TokenVar s)

tkInt :: [Char] -> Parser Token
tkInt s =
  pure (TokenInt (read s))

tkBas :: [Char] -> Parser Token
tkBas _ =
  pure TokenBas

tkCol :: [Char] -> Parser Token
tkCol _ =
  pure TokenCol

tkCom :: [Char] -> Parser Token
tkCom _ =
  pure TokenCom

tkDot :: [Char] -> Parser Token
tkDot _ =
  pure TokenDot

tkHepgar :: [Char] -> Parser Token
tkHepgar _ =
  pure TokenHepgar

tkKel :: [Char] -> Parser Token
tkKel _ =
  pure TokenKel

tkKer :: [Char] -> Parser Token
tkKer _ =
  pure TokenKer

tkPal :: [Char] -> Parser Token
tkPal _ =
  pure TokenPal

tkPar :: [Char] -> Parser Token
tkPar _ =
  pure TokenPar

tkSem :: [Char] -> Parser Token
tkSem _ =
  pure TokenSem
