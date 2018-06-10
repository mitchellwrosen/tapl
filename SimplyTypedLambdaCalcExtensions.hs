module SimplyTypedLambdaCalcExtensions where

import Bound
import Control.Monad
import Control.Monad.Trans
import Data.Foldable (asum)
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import Data.Void
import GHC.Generics (Generic1)
import Text.Megaparsec
import Text.Megaparsec.Char

--------------------------------------------------------------------------------
-- Term
--------------------------------------------------------------------------------

data Term a
  = TermVar a
  | TermLam Type (Scope () Term a)
  | TermApp (Term a) (Term a)
  | TermTrue
  | TermFalse
  | TermIf (Term a) (Term a) (Term a)
  deriving (Foldable, Functor, Generic1, Show, Traversable)

instance Applicative Term where
  pure = TermVar
  (<*>) = ap

instance Monad Term where
  return = pure
  TermVar x >>= f = f x
  TermLam y x >>= f = TermLam y (x >>= lift . f)
  TermApp x y >>= f = TermApp (x >>= f) (y >>= f)
  TermTrue >>= _ = TermTrue
  TermFalse >>= _ = TermFalse
  TermIf t1 t2 t3 >>= f = TermIf (t1 >>= f) (t2 >>= f) (t3 >>= f)

instance Show1 Term where
  liftShowsPrec = liftShowsPrecDefault

pattern Value :: Term a -> Term a
pattern Value t <- (matchValue -> Just t)

matchValue :: Term a -> Maybe (Term a)
matchValue = \case
  t@TermLam{} -> Just t
  _ -> Nothing

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

data Type
  = TypeFun Type Type
  | TypeBool
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Context
--------------------------------------------------------------------------------

type Context a
  = [(a, Type)]

--------------------------------------------------------------------------------
-- Type checking
--------------------------------------------------------------------------------

typeOf :: Term Void -> Maybe Type
typeOf =
  typeOf' . vacuous

typeOf' :: Term Type -> Maybe Type
typeOf' = \case
  TermVar x ->
    Just x
  TermLam y t -> do
    r <- typeOf' (instantiate1 (TermVar y) t)
    pure (TypeFun y r)
  TermApp t1 t2 -> do
    TypeFun y1 y2 <- typeOf' t1
    z <- typeOf' t2
    guard (y1 == z)
    pure y2
  TermTrue ->
    pure TypeBool
  TermFalse ->
    pure TypeBool
  TermIf t1 t2 t3 -> do
    TypeBool <- typeOf' t1
    y2 <- typeOf' t2
    y3 <- typeOf' t3
    guard (y2 == y3)
    pure y2

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

-- | Call-by-value big-step evaluation.
eval :: Term a -> Term a
eval term =
  maybe term eval (eval1 term)

-- | Call-by-value small-step evaluation.
eval1 :: Term a -> Maybe (Term a)
eval1 = \case
  TermApp (TermLam _ t) (Value v) ->
   Just (instantiate1 v t)
  TermApp (Value v1) t2 ->
    TermApp v1 <$> eval1 t2
  TermApp t1 t2 ->
    TermApp <$> eval1 t1 <*> pure t2
  TermIf TermTrue t _ ->
    Just t
  TermIf TermFalse _ t ->
    Just t
  TermIf t1 t2 t3 ->
    TermIf <$> eval1 t1 <*> pure t2 <*> pure t3
  TermLam{} ->
    Nothing
  TermVar{} ->
    Nothing
  TermTrue ->
    Nothing
  TermFalse ->
    Nothing

evalString :: [Char] -> Either (ParseError Char ()) (Maybe (Term Void))
evalString =
  fmap f . parseTerm
 where
  f :: Term [Char] -> Maybe (Term Void)
  f t = do
    t' <- closed t
    _ <- typeOf t'
    pure (eval t')

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

type Parser a
  = Parsec () [Char] a

parseTerm :: [Char] -> Either (ParseError Char ()) (Term [Char])
parseTerm s =
  runParser termParser "" s

termParser :: Parser (Term [Char])
termParser =
  foldl1 TermApp <$> some (lambdaParser <|> varParser <|> parens termParser)
 where
  lambdaParser :: Parser (Term [Char])
  lambdaParser = do
    _ <- char '\\' <* space
    v <- some lowerChar <* space
    _ <- char ':' <* space
    y <- typeParser
    _ <- char '.' <* space
    t <- termParser
    pure (TermLam y (abstract1 v t))

  varParser :: Parser (Term [Char])
  varParser =
    TermVar <$> some lowerChar <* space

typeParser :: Parser Type
typeParser = do
  ty1 <- atomParser
  optional (string "->" *> space) >>= \case
    Nothing ->
      pure ty1
    Just _ -> do
      TypeFun ty1 <$> typeParser
 where
  atomParser :: Parser Type
  atomParser =
    asum
      [ TypeBool <$ string "bool" <* space
      , parens typeParser
      ]

parens :: Parser a -> Parser a
parens p =
  char '(' *> p <* char ')' <* space
