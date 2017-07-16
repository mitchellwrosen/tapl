--------------------------------------------------------------------------------
-- Module header

{
module GenLexer where

import GenLexerLib
}

--------------------------------------------------------------------------------
-- Wrapper

%wrapper "basic"

--------------------------------------------------------------------------------
-- Macro definitions

$digit     = 0-9
$alpha     = [a-zA-Z]

@ident = $alpha ($alpha | $digit)*

--------------------------------------------------------------------------------
-- Tokens

:-

$white+ ;

bool   { tkBool      }
true   { tkTrue      }
false  { tkFalse     }

@ident { tkIdent     }

\\     { tkBackslash }
\.     { tkDot       }
:      { tkColon     }
\->    { tkArrow     }
\(     { tkLparen    }
\)     { tkRparen    }
