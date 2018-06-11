{

module Stlc.Alex where

import Stlc.Token

import Data.ByteString (ByteString)
import Data.Word

import qualified Data.ByteString as ByteString

}

:-

$white+ ;

as    { \_ -> TokenAs     }
bool  { \_ -> TokenBool   }
false { \_ -> TokenFalse  }
true  { \_ -> TokenTrue   }
unit  { \_ -> TokenUnit   }

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
