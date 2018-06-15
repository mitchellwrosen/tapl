{

module Stlc.Alex where

import Stlc.Lexer

}

:-

$white+ ;

as    { tkAs     }
bool  { tkBool   }
false { tkFalse  }
in    { tkIn     }
let   { tkLet    }
true  { tkTrue   }
unit  { tkUnit   }

a-z+  { tkVar    }
0-9+  { tkInt    }

\\    { tkBas    }
:     { tkCol    }
\,    { tkCom    }
\.    { tkDot    }
\->   { tkHepgar }
\{    { tkKel    }
\}    { tkKer    }
\(    { tkPal    }
\)    { tkPar    }
\;    { tkSem    }
=     { tkTis    }
