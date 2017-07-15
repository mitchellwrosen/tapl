{
module GenParser where

import GenParserLib
}

%name parse

%tokentype { Token }

%error { perror }

%token

  var    { TkVar $$ }
  '\\'   { TkLam    }
  '.'    { TkDot    }
  '('    { TkLParen }
  ')'    { TkRParen }

%%

Term
  : '\\' var '.' Term { onLam $2 $4 }
  | Term1             { $1          }

Term1
  : Term2             { $1          }
  | Term1 Term2       { onApp $1 $2 }

Term2
  : var               { onVar $1    }
  | '(' Term ')'      { $2          }
