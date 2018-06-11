{
module Stlc.Happy where

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
  '.'   { TokenDot    }
  '->'  { TokenHepGar }
  '('   { TokenPal    }
  ')'   { TokenPar    }
  ';'   { TokenSem    }
  as    { TokenAs     }
  bool  { TokenBool   }
  false { TokenFalse  }
  true  { TokenTrue   }
  unit  { TokenBool   }
  var   { TokenVar $$ }

%right '->' ';'
%right '.'

%nonassoc '\\' '(' var unit true false
%nonassoc APP
%nonassoc as

%%

Term
  : var                        { TermVar $1                              }
  | '\\' var ':' Type '.' Term { TermLam $4 (abstract1 $2 $6)            }
  | Term Term %prec APP        { TermApp $1 $2                           }
  | Term ';' Term              { TermApp (TermLam TypeUnit (lift $3)) $1 }
  | Term as Type               { TermAs $1 $3                            }
  | '(' Term ')'               { $2                                      }
  | unit                       { TermUnit                                }
  | true                       { TermTrue                                }
  | false                      { TermFalse                               }

Type
  : bool           { TypeBool      }
  | unit           { TypeUnit      }
  | Type '->' Type { TypeFun $1 $3 }

{
parseError :: [Token] -> a
parseError = error . show
}
