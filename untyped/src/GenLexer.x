{
module GenLexer where

import GenLexerLib
}

%wrapper "basic"

$digit     = 0-9
$alpha     = [a-zA-Z]
$backslash = \\

@ident = $alpha ($alpha | $digit)*

:-

  $white+ ;

  @ident     { onIdent }

  $backslash { onBackslash }
  "."        { onDot       }
  "("        { onLParen    }
  ")"        { onRParen    }
