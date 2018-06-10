module SimplyTypedLambdaCalcExtensions where

import Bound
import Control.Applicative hiding (some)
import Control.Monad
import Control.Monad.Trans
import Data.Bifunctor
import Data.Foldable (asum)
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import Data.Void
import GHC.Generics (Generic1)
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Text.Prettyprint.Doc as Ppr
import qualified Data.Text.Prettyprint.Doc.Render.Text as Ppr

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
  | TermUnit
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
  TermUnit >>= _ = TermUnit

instance Show1 Term where
  liftShowsPrec = liftShowsPrecDefault

pattern Value :: Term a -> Term a
pattern Value t <- (matchValue -> Just t)

matchValue :: Term a -> Maybe (Term a)
matchValue = \case
  t@TermLam{} -> Just t
  TermTrue -> Just TermTrue
  TermFalse -> Just TermFalse
  TermUnit -> Just TermUnit

  TermVar{} -> Nothing
  TermApp{} -> Nothing
  TermIf{} -> Nothing

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

data Type
  = TypeFun Type Type
  | TypeBool
  | TypeUnit
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Type checking
--------------------------------------------------------------------------------

typeOf :: Term Void -> Maybe Type
typeOf =
  typeOf' . vacuous

typeOf' :: Term Type -> Maybe Type
typeOf' = \case
  TermVar x ->
    pure x
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
  TermUnit ->
    pure TypeUnit

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

  TermLam{} -> Nothing
  TermVar{} -> Nothing
  TermTrue -> Nothing
  TermFalse -> Nothing
  TermUnit -> Nothing

evalString :: [Char] -> Either String (Maybe (Term Void))
evalString =
  bimap parseErrorPretty f . parseTerm
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
  = Parsec Void [Char] a

parseTerm :: [Char] -> Either (ParseError Char Void) (Term [Char])
parseTerm s =
  runParser termParser "" s


-- term
--   = true term'
--   | false term'
--   | unit term'
--   | ( term ) term'
--   | \ var : type . term term'
--   | var term'
--
-- term'
--   = term term'
--   | ; term term'
--   | empty
termParser :: Parser (Term [Char])
termParser =
  foldl1 TermApp <$>
    some
      (asum
        [ (TermTrue <$ lexeme (string "true")) <**> tailParser
        , (TermFalse <$ lexeme (string "false")) <**> tailParser
        , (TermUnit <$ lexeme (string "unit")) <**> tailParser
        , (TermVar <$> lexeme (some lowerChar)) <**> tailParser
        , lambdaParser <**> tailParser
        , parens termParser <**> tailParser
        ])
 where
  tailParser :: Parser (Term [Char] -> Term [Char])
  tailParser =
    asum
      [ do
          _ <- lexeme (char ';')
          x <- termParser
          f <- tailParser
          pure (\t -> f (TermApp (TermLam TypeUnit (lift x)) t))
      , do
          x <- termParser
          f <- tailParser
          pure
            (\t ->
              case x of
                TermApp v1 v2 -> f (TermApp (TermApp t v1) v2)
                _ -> f (TermApp t x))
      , pure id
      ]

  lambdaParser :: Parser (Term [Char])
  lambdaParser = do
    _ <- lexeme (char '\\')
    v <- lexeme (some lowerChar)
    _ <- lexeme (char ':')
    y <- typeParser
    _ <- lexeme (char '.')
    t <- termParser
    pure (TermLam y (abstract1 v t))

typeParser :: Parser Type
typeParser = do
  ty1 <- atomParser
  optional (lexeme (string "->")) >>= \case
    Nothing ->
      pure ty1
    Just _ -> do
      TypeFun ty1 <$> typeParser
 where
  atomParser :: Parser Type
  atomParser =
    asum
      [ TypeBool <$ lexeme (string "bool")
      , TypeUnit <$ lexeme (string "unit")
      , parens typeParser
      ]

parens :: Parser a -> Parser a
parens p =
  lexeme (char '(' *> p <* char ')')

lexeme :: Parser a -> Parser a
lexeme =
  (<* space)

--------------------------------------------------------------------------------
-- Pretty-printing
--------------------------------------------------------------------------------

-- Horrible pretty-printing functions for debugging. They don't work properly.

putTerm :: Term [Char] -> IO ()
putTerm =
  Ppr.putDoc . pprTerm

pprTerm :: Term [Char] -> Ppr.Doc ()
pprTerm = \case
  TermVar s -> Ppr.pretty s
  TermLam y t ->
    "(\\_ : " <> pprType y <> ". " <> pprTerm (instantiate1 (TermVar "x") t)
      <> ")"
  TermApp t1 t2 -> pprTerm t1 <> Ppr.space <> pprTerm t2
  TermTrue -> "true"
  TermFalse -> "false"
  TermIf t1 t2 t3 ->
    "if " <> pprTerm t1 <> " then " <> pprTerm t2 <> " else " <> pprTerm t3
  TermUnit -> "unit"

pprType :: Type -> Ppr.Doc ()
pprType = \case
  TypeBool -> "bool"
  TypeUnit -> "unit"
  TypeFun t1 t2 -> pprType t1 <> " -> " <> pprType t2
