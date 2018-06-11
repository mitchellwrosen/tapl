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

Term
  : var                        { TermVar $1                              }
  | '\\' var ':' Type '.' Term { TermLam $4 (abstract1 $2 $6)            }
  | Term Term %prec APP        { TermApp $1 $2                           }
  | Term ';' Term              { TermApp (TermLam TypeUnit (lift $3)) $1 }
  | Term as Type               { TermAs $1 $3                            }
  | '{' '}'                    { TermTuple []                            }
  | '{' Terms '}'              { TermTuple (reverse $2)                  }
  | Term '.' int               { TermTupleIx $1 $3                       }
  | let var '=' Term in Term   { TermLet $4 (abstract1 $2 $6)            }
  | '(' Term ')'               { $2                                      }
  | unit                       { TermUnit                                }
  | true                       { TermTrue                                }
  | false                      { TermFalse                               }

Terms
  : Term           { [$1] }
  | Terms ',' Term { $3 : $1 }

Type
  : bool           { TypeBool      }
  | unit           { TypeUnit      }
  | Type '->' Type { TypeFun $1 $3 }

{
parseError :: [Token] -> a
parseError = error . show
}


