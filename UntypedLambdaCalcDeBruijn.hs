module UntypedLambdaCalcDeBruijn where

import Control.Monad.Reader
import Data.Maybe
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char

--------------------------------------------------------------------------------
-- Term
--------------------------------------------------------------------------------

type Var
  = Int

data Term
  = Var Var
  | Lam Term
  | App Term Term
  deriving Show

pattern Value :: Term -> Term
pattern Value t <- (matchValue -> Just t)

matchValue :: Term -> Maybe Term
matchValue = \case
  t@Lam{} -> Just t
  _ -> Nothing

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

-- | Call-by-value big-step evaluation.
eval :: Term -> Term
eval term =
  maybe term eval (eval1 term)

-- | Call-by-value small-step evaluation.
eval1 :: Term -> Maybe Term
eval1 = \case
  App (Lam t) (Value v) ->
    Just (shift (-1) (subst 0 (shift 1 v) t))
  App (Value v1) t2 ->
    App v1 <$> eval1 t2
  App t1 t2 ->
    App <$> eval1 t1 <*> pure t2
  Lam{} ->
    Nothing
  Var{} ->
    Nothing

evalString :: [Char] -> Either (ParseError Char ()) Term
evalString =
  fmap eval . parseTerm

--------------------------------------------------------------------------------
-- Substitution
--------------------------------------------------------------------------------

-- | @subst x t s@ substitutes occurrences of @x@ in @s@ to @t@.
subst :: Var -> Term -> Term -> Term
subst x t = \case
  Var y ->
    if x == y
      then t
      else Var y
  Lam s ->
    Lam (subst (x+1) (shift 1 t) s)
  App t1 t2 ->
    App (subst x t t1) (subst x t t2)

-- | @shift d t@ shifts all variables by @d@.
shift :: Int -> Term -> Term
shift d t =
  shift_ d 0 t

-- | @shift_ d c t@ shifts all variables @>= c@ by @d@.
shift_ :: Int -> Var -> Term -> Term
shift_ d c = \case
  Var v | v >= c ->
    Var (v+d)
  Var v ->
    Var v
  Lam t ->
    Lam (shift_ d (c+1) t)
  App t1 t2 ->
    App (shift_ d c t1) (shift_ d c t2)

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

type Parser a
  = ParsecT () [Char] (Reader [[Char]]) a

parseTerm :: [Char] -> Either (ParseError Char ()) Term
parseTerm s =
  runReader (runParserT termParser "" s) []

termParser :: Parser Term
termParser =
  foldl1 App <$> some (lambdaParser <|> varParser <|> parenParser)
 where
  lambdaParser :: Parser Term
  lambdaParser = do
    _ <- char '\\' <* space
    v <- some lowerChar <* space
    _ <- char '.' <* space
    t <- local (++ [v]) termParser <* space
    pure (Lam t)

  varParser :: Parser Term
  varParser = do
    v <- some lowerChar <* space
    Var <$> lookupVar v

  parenParser :: Parser Term
  parenParser =
    char '(' *> space *> termParser <* space <* char ')' <* space

  lookupVar :: [Char] -> Parser Var
  lookupVar v = do
    fromJust . elemIndex v <$> ask
