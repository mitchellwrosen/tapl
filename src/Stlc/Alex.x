{

module Stlc.Alex where

import Stlc.Lexer

}

:-

$white+ ;

as    { tkAs     }
bool  { tkBool   }
case  { tkCase   }
false { tkFalse  }
in    { tkIn     }
let   { tkLet    }
of    { tkOf     }
true  { tkTrue   }
unit  { tkUnit   }

a-z+  { tkVar    }
0-9+  { tkInt    }

\\    { tkBas    }
:     { tkCol    }
\,    { tkCom    }
\.    { tkDot    }
\<    { tkGal    }
>     { tkGar    }
\->   { tkHepgar }
\{    { tkKel    }
\}    { tkKer    }
\(    { tkPal    }
\)    { tkPar    }
\;    { tkSem    }
\=    { tkTis    }
\=\>  { tkTisgar }
