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
  var   { TokenVar $$ }
  unit  { TokenBool   }
  bool  { TokenBool   }
  true  { TokenTrue   }
  false { TokenFalse  }

%right '->' ';'
%right '.'

-- Here we list all of the tokens that may begin a Term. The associativity is
-- not important - we only wish to give them less precedence than the
-- pseudo-token APP. This resolves 'f x y' to '(f x) y'.
%nonassoc '\\' '(' var unit true false
%nonassoc APP

%%

Term
  : var                        { TermVar $1                              }
  | '\\' var ':' Type '.' Term { TermLam $4 (abstract1 $2 $6)            }
  | Term Term %prec APP        { TermApp $1 $2                           }
  | unit                       { TermUnit                                }
  | true                       { TermTrue                                }
  | false                      { TermFalse                               }
  | Term ';' Term              { TermApp (TermLam TypeUnit (lift $3)) $1 }
  | '(' Term ')'               { $2                                      }

Type
  : bool           { TypeBool      }
  | unit           { TypeUnit      }
  | Type '->' Type { TypeFun $1 $3 }

{
parseError :: [Token] -> a
parseError = error . show
}
