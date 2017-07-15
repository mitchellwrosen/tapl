module Token where

data Token
  = TkVar String -- x
  | TkLam        -- \
  | TkDot        -- .
  | TkLParen     -- (
  | TkRParen     -- )
  deriving (Eq, Show )
