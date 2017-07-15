module Parser
  ( Parser.parse
  ) where

import Lexer
import Term
import Token

import Data.Loc
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim

parse :: String -> Either ParseError Term
parse = Text.Parsec.Prim.parse parser "" . lexer

satisfy :: (L Token -> Bool) -> Parsec [L Token] () (L Token)
satisfy p = tokenPrim show next (\tk -> if p tk then Just tk else Nothing)
 where
  next :: SourcePos -> L Token -> [L Token] -> SourcePos
  next _ _ (L (Loc (Pos name line col _) _) _ : _) = newPos name line col
  next pos _ _ = pos

parser :: Parsec [L Token] () Term
parser = do
  tkLam
  Loc _ v <- tkVar
  tkDot
  pure (

tkVar :: Parser [L Token] () (L Token)
tkVar =
  satisfy (\tk -> case unLoc tk of
                    TkVar _ -> True
                    _ -> False)

tkDot :: Parser [L Token] () ()
tkDot = do
  satisfy (\tk -> unLoc tk == TkDot)
  pure ()

tkLam :: Parser [L Token] () ()
tkLam = do
  satisfy (\tk -> unLoc tk == TkLam)
  pure ()
