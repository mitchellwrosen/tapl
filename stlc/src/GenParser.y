--------------------------------------------------------------------------------
-- Module header

{
module GenParser where

import GenParserLib
import Term (Term)
import Token (Token(..))
import Type (Type(..))
}

--------------------------------------------------------------------------------
-- Parser names

%name parseTerm term
%name parseType type

--------------------------------------------------------------------------------
-- Token type

%tokentype { Token }

--------------------------------------------------------------------------------
-- Error declaration

%error { perror }

--------------------------------------------------------------------------------
-- Tokens (terminal symbols)

%token

IDENT  { TkIdent $$ }
BOOL   { TkBool }
TRUE   { TkTrue }
FALSE  { TkFalse }
'\\'   { TkLam }
'.'    { TkDot }
':'    { TkColon }
'->'   { TkArrow }
'('    { TkLParen }
')'    { TkRParen }

--------------------------------------------------------------------------------
-- Grammar

%%

term :: { Term }
  : '\\' IDENT ':' type '.' term { lam $2 $4 $6 }
  | term1 { $1 }

term1 :: { Term }
  : term2 { $1 }
  | term1 term2 { app $1 $2 }

term2 :: { Term }
  : IDENT { var $1 }
  | TRUE { true }
  | FALSE { false }
  | '(' term ')' { $2 }

type :: { Type }
  : BOOL { TyBool }
  | type '->' type { TyArrow $1 $3 }
