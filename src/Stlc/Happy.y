{

module Stlc.Happy where

import Stlc.Label
import Stlc.Lexer (Parser)
import {-# SOURCE #-} Stlc.Parser
import Stlc.Term
import Stlc.Token
import Stlc.Type

import Bound
import Control.Monad.Trans

import qualified Data.List.NonEmpty as List1

}

%name parser
%tokentype { Token }
%monad { Parser }
%lexer { lexer } { TokenEOF }
%error { parseError }

%token
  '\\'  { TokenBas    }
  ':'   { TokenCol    }
  ','   { TokenCom    }
  '.'   { TokenDot    }
  '<'   { TokenGal    }
  '>'   { TokenGar    }
  '->'  { TokenHepgar }
  '{'   { TokenKel    }
  '}'   { TokenKer    }
  '('   { TokenPal    }
  ')'   { TokenPar    }
  ';'   { TokenSem    }
  '='   { TokenTis    }
  '=>'  { TokenTisgar }
  as    { TokenAs     }
  bool  { TokenBool   }
  case  { TokenCase   }
  false { TokenFalse  }
  in    { TokenIn     }
  let   { TokenLet    }
  of    { TokenOf     }
  true  { TokenTrue   }
  unit  { TokenUnit   }
  var   { TokenVar $$ }
  int   { TokenInt $$ }

%right '->' ';'
%right '.'
%right in

%nonassoc '\\' '(' '{' '<' as case let false true unit var
%nonassoc ALT APP

%%

Term :: { Term [Char] }
Term
  : var                          { TermVar $1                                }
  | '\\' var ':' Type '.' Term   { TermLam $4 (abstract1 $2 $6)              }
  | Term Term %prec APP          { TermApp $1 $2                             }
  | Term ';' Term                { TermApp (TermLam TypeUnit (lift $3)) $1   }
  | Term as Type                 { TermAs $1 $3                              }
  | '{' '}'                      { TermTuple []                              }
  | '{' Tuple '}'                { TermTuple (reverse $2)                    }
  | Term '.' int                 { TermTupleIx $1 $3                         }
  | '{' Record '}'               { TermRecord (reverse $2)                   }
  | '<' var '=' Term '>' as Type { TermVariant $2 $4 $7                      }
  | case Term of '{' Alts '}'    { TermCase $2 (List1.fromList (reverse $5)) }
  | let var '=' Term in Term     { TermLet $4 (abstract1 $2 $6)              }
  | '(' Term ')'                 { $2                                        }
  | unit                         { TermUnit                                  }
  | true                         { TermTrue                                  }
  | false                        { TermFalse                                 }

Tuple :: { [Term [Char]] }
Tuple
  : Term           { [$1] }
  | Tuple ',' Term { $3 : $1 }

Record :: { [(Label, Term [Char])] }
Record
  : var '=' Term            { [($1, $3)] }
  | Record ',' var '=' Term { ($3, $5) : $1 }

Type :: { Type }
Type
  : bool                { TypeBool                                  }
  | unit                { TypeUnit                                  }
  | '<' TypeVariant '>' { TypeVariant (List1.fromList (reverse $2)) }
  | Type '->' Type      { TypeFun $1 $3                             }

TypeVariant
  : TypeVariant1                 { [$1]    }
  | TypeVariant ',' TypeVariant1 { $3 : $1 }

TypeVariant1
  : var ':' Type { ($1, $3) }

Alts
  : Alts1      { [$1]    }
  | Alts Alts1 { $2 : $1 }

Alts1
  : '<' var '=' var '>' '=>' Term ';' %prec ALT { ($2, abstract1 $4 $7) }
