module GenLexerLib where

import Token

onIdent :: String -> Token
onIdent = TkVar

onBackslash :: String -> Token
onBackslash _ = TkLam

onDot :: String -> Token
onDot _ = TkDot

onLParen :: String -> Token
onLParen _ = TkLParen

onRParen :: String -> Token
onRParen _ = TkRParen
