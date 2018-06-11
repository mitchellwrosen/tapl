{
module Stlc.Happy where

import Stlc.Label
import Stlc.Term
import Stlc.Token
import Stlc.Type

import Bound
import Control.Monad.Trans
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  '\\'  { TokenBas    }
  ':'   { TokenCol    }
  ','   { TokenCom    }
  '.'   { TokenDot    }
  '->'  { TokenHepGar }
  '{'   { TokenKel    }
  '}'   { TokenKer    }
  '('   { TokenPal    }
  ')'   { TokenPar    }
  ';'   { TokenSem    }
  '='   { TokenTis    }
  as    { TokenAs     }
  bool  { TokenBool   }
  false { TokenFalse  }
  in    { TokenIn     }
  let   { TokenLet    }
  true  { TokenTrue   }
  unit  { TokenBool   }
  var   { TokenVar $$ }
  int   { TokenInt $$ }

%right '->' ';'
%right '.'
%right in

%nonassoc '\\' '(' '{' as let false true unit var
%nonassoc APP

%%

Term :: { Term [Char] }
Term
  : var                        { TermVar $1                              }
  | '\\' var ':' Type '.' Term { TermLam $4 (abstract1 $2 $6)            }
  | Term Term %prec APP        { TermApp $1 $2                           }
  | Term ';' Term              { TermApp (TermLam TypeUnit (lift $3)) $1 }
  | Term as Type               { TermAs $1 $3                            }
  | '{' '}'                    { TermTuple []                            }
  | '{' Tuple '}'              { TermTuple (reverse $2)                  }
  | '{' Record '}'             { TermRecord (reverse $2)                 }
  | Term '.' int               { TermTupleIx $1 $3                       }
  | let var '=' Term in Term   { TermLet $4 (abstract1 $2 $6)            }
  | '(' Term ')'               { $2                                      }
  | unit                       { TermUnit                                }
  | true                       { TermTrue                                }
  | false                      { TermFalse                               }

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
  : bool           { TypeBool      }
  | unit           { TypeUnit      }
  | Type '->' Type { TypeFun $1 $3 }

{
parseError :: [Token] -> a
parseError = error . show
}


