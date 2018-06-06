module GenLexerLib where

import Token

tkBool :: String -> Token
tkBool _ = TkBool

tkTrue :: String -> Token
tkTrue _ = TkTrue

tkFalse :: String -> Token
tkFalse _ = TkFalse

tkIdent :: String -> Token
tkIdent = TkIdent

tkBackslash :: String -> Token
tkBackslash _ = TkLam

tkDot :: String -> Token
tkDot _ = TkDot

tkColon :: String -> Token
tkColon _ = TkColon

tkArrow :: String -> Token
tkArrow _ = TkArrow

tkLparen :: String -> Token
tkLparen _ = TkLParen

tkRparen :: String -> Token
tkRparen _ = TkRParen
