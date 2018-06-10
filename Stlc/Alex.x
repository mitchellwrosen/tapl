{

module Stlc.Alex where

import Stlc.Token

import Data.ByteString (ByteString)
import Data.Word

import qualified Data.ByteString as ByteString

}

:-

$white+ ;

unit  { \_ -> TokenUnit   }
bool  { \_ -> TokenBool   }
true  { \_ -> TokenTrue   }
false { \_ -> TokenFalse  }
a-z+  { \s -> TokenVar s  }

\- >  { \_ -> TokenHepGar }
\\    { \_ -> TokenBas    }
:     { \_ -> TokenCol    }
\.    { \_ -> TokenDot    }
\(    { \_ -> TokenPal    }
\)    { \_ -> TokenPar    }
\;    { \_ -> TokenSem    }

{

type AlexInput
  = ByteString

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte =
  ByteString.uncons

}
