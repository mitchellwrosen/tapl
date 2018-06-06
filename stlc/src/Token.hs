module Token where

data Token
  = TkIdent String
  | TkBool         -- bool
  | TkTrue         -- true
  | TkFalse        -- false
  | TkLam          -- \
  | TkDot          -- .
  | TkColon        -- :
  | TkArrow        -- ->
  | TkLParen       -- (
  | TkRParen       -- )
  deriving (Eq, Show)
