module UntypedExpr where

import Control.Monad
import Data.Char (isSpace)
import Data.Foldable (asum)
import Data.Loc (L(L), Loc(Loc, NoLoc), Pos(Pos))
import Language.Lexer.Applicative (Lexer)
import Prelude hiding (lex)
import Text.Parsec (Parsec, ParseError)

import qualified Language.Lexer.Applicative as Lexer
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Pos as Parsec
import qualified Text.Regex.Applicative as Regex

--------------------------------------------------------------------------------
-- Term
-------------------------------------------------------------------------------

data Term
  = TermTrue
  | TermFalse
  | TermIf Term Term Term
  | TermZero
  | TermSucc Term
  | TermPred Term
  | TermIsZero Term
  deriving Show

pattern NumericValue :: Term -> Term
pattern NumericValue t <- (matchNumericValue -> Just t)

matchNumericValue :: Term -> Maybe Term
matchNumericValue = \case
  TermZero ->
    Just TermZero
  TermSucc t ->
    TermSucc <$> matchNumericValue t
  _ ->
    Nothing

--------------------------------------------------------------------------------
-- Evaluation
-------------------------------------------------------------------------------

eval :: Term -> Term
eval term =
  maybe term eval (eval1 term)

eval1 :: Term -> Maybe Term
eval1 = \case
  TermIf TermTrue t _ ->
    Just t
  TermIf TermFalse _ t ->
    Just t
  TermIf t1 t2 t3 ->
    TermIf
      <$> eval1 t1
      <*> pure t2
      <*> pure t3
  TermSucc t ->
    TermSucc <$> eval1 t
  TermPred TermZero ->
    Just TermZero
  TermPred (TermSucc (NumericValue t)) ->
    Just t
  TermPred t ->
    TermPred
      <$> eval1 t
  TermIsZero TermZero ->
    Just TermTrue
  TermIsZero (TermSucc (NumericValue _)) ->
    Just TermFalse
  TermIsZero t ->
    TermIsZero
      <$> eval1 t
  TermZero ->
    Nothing
  TermTrue ->
    Nothing
  TermFalse ->
    Nothing

evalString :: [Char] -> Either ParseError Term
evalString =
  fmap eval . parse . lex

--------------------------------------------------------------------------------
-- Token
--------------------------------------------------------------------------------

data Token
  = TokenTrue
  | TokenFalse
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenZero
  | TokenSucc
  | TokenPred
  | TokenIsZero
  | TokenLParen
  | TokenRParen
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Lexing
--------------------------------------------------------------------------------

lex :: [Char] -> [L Token]
lex =
  Lexer.streamToList . Lexer.runLexer lexer ""

lexer :: Lexer Token
lexer =
  mconcat
    [ Lexer.token (Lexer.longest reToken)
    , Lexer.whitespace (Lexer.longest (Regex.psym isSpace))
    ]

reToken :: Regex.RE Char Token
reToken =
  asum
    [ TokenTrue   <$ "true"
    , TokenFalse  <$ "false"
    , TokenIf     <$ "if"
    , TokenThen   <$ "then"
    , TokenElse   <$ "else"
    , TokenZero   <$ "0"
    , TokenSucc   <$ "succ"
    , TokenPred   <$ "pred"
    , TokenIsZero <$ "iszero"
    , TokenLParen <$ "("
    , TokenRParen <$ ")"
    ]

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

type Parser a
  = Parsec [L Token] () a

parse :: [L Token] -> Either ParseError Term
parse =
  Parsec.parse termParser ""

termParser :: Parser Term
termParser =
  asum
    [ TermTrue <$ tk TokenTrue
    , TermFalse <$ tk TokenFalse
    , TermIf
        <$> (tk TokenIf *> termParser)
        <*> (tk TokenThen *> termParser)
        <*> (tk TokenElse *> termParser)
    , TermZero <$ tk TokenZero
    , TermSucc <$> (tk TokenSucc *> termParser)
    , TermPred <$> (tk TokenPred *> termParser)
    , TermIsZero <$> (tk TokenIsZero *> termParser)
    , tk TokenLParen *> termParser <* tk TokenRParen
    ]

tk :: Token -> Parser ()
tk t =
  Parsec.token
    show
    (\case
      L (Loc (Pos file line col _) _) _ ->
        Parsec.newPos file line col
      L NoLoc _ ->
        error "NoLoc")
    (\(L _ t') -> guard (t == t'))
