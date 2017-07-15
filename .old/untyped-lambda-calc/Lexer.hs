module Lexer where

import Token

import Data.Char
import Data.Foldable
import Data.Loc
import Language.Lexer.Applicative
import Text.Regex.Applicative

import qualified Data.Text as Text

lexer :: String -> [L Token]
lexer = streamToList . runLexer (token (longest tok)) ""
 where
  tok :: RE Char Token
  tok = asum
    [ TkVar . Text.pack <$> some (psym isAlpha)
    , TkLam             <$  sym '\\'
    , TkSpace           <$  some (psym isSpace)
    , TkDot             <$  sym '.'
    , TkLParen          <$  sym '('
    , TkRParen          <$  sym ')'
    ]
