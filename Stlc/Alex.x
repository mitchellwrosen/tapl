{

module Stlc.Alex where

import Stlc.Token

import Data.ByteString (ByteString)
import Data.Word

import qualified Data.ByteString as ByteString

}

:-

$white+ ;

as    { \_ -> TokenAs           }
bool  { \_ -> TokenBool         }
false { \_ -> TokenFalse        }
in    { \_ -> TokenIn           }
let   { \_ -> TokenLet          }
true  { \_ -> TokenTrue         }
unit  { \_ -> TokenUnit         }

a-z+  { \s -> TokenVar s        }
0-9+  { \s -> TokenInt (read s) }

\- >  { \_ -> TokenHepGar       }
\\    { \_ -> TokenBas          }
:     { \_ -> TokenCol          }
\,    { \_ -> TokenCom          }
\.    { \_ -> TokenDot          }
\{    { \_ -> TokenKel          }
\}    { \_ -> TokenKer          }
\(    { \_ -> TokenPal          }
\)    { \_ -> TokenPar          }
\;    { \_ -> TokenSem          }
=     { \_ -> TokenTis          }

{

type AlexInput
  = ByteString

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte =
  ByteString.uncons

}
