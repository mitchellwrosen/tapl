module UntypedLambdaCalc where

import Data.Set (Set)
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Term
--------------------------------------------------------------------------------

type Var
  = [Char]

data Term
  = Var Var
  | Lam Var Term
  | App Term Term
  deriving Show

pattern Value :: Term -> Term
pattern Value t <- (matchValue -> Just t)

matchValue :: Term -> Maybe Term
matchValue = \case
  t@Lam{} -> Just t
  _ -> Nothing

free :: Term -> Set Var
free = \case
  Var x ->
    Set.singleton x
  Lam x t ->
    Set.delete x (free t)
  App t1 t2 ->
    free t1 <> free t2

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
  App (Lam x t) (Value v2) ->
    Just (subst x v2 t)
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

-- | @subst x t s@ substitutes free occurrences of @x@ in @s@ to @t@.
subst :: Var -> Term -> Term -> Term
subst x t = \case
  Var y ->
    if x == y
      then t
      else Var y
  Lam y s
    | x == y ->
        Lam y s
    | y `notElem` free t ->
        Lam y (subst x t s)
    | otherwise ->
        let
          y' = y ++ "#"
        in
          Lam y' (subst x t (rename y y' s))
  App t1 t2 ->
    App (subst x t t1) (subst x t t2)

--------------------------------------------------------------------------------
-- Renaming
--------------------------------------------------------------------------------

-- | @rename x y t@ renames free occurrences of @x@ to @y@.
rename :: Var -> Var -> Term -> Term
rename v w = \case
  Var x ->
    if x == v
      then Var w
      else Var x
  Lam x t ->
    if x == v
      then Lam x t
      else Lam x (rename v w t)
  App t1 t2 ->
    App (rename v w t1) (rename v w t2)

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

type Parser a
  = Parsec () [Char] a

parseTerm :: [Char] -> Either (ParseError Char ()) Term
parseTerm =
  parse termParser ""

termParser :: Parser Term
termParser =
  foldl1 App <$> some (lambdaParser <|> varParser <|> parenParser)
 where
  lambdaParser :: Parser Term
  lambdaParser = do
    _ <- char '\\' <* space
    x <- some lowerChar <* space
    _ <- char '.' <* space
    t <- termParser <* space
    pure (Lam x t)

  varParser :: Parser Term
  varParser =
    Var <$> (some lowerChar <* space)

  parenParser :: Parser Term
  parenParser =
    char '(' *> space *> termParser <* space <* char ')' <* space
