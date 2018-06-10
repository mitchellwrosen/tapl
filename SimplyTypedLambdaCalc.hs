{-# language ApplicativeDo #-}

module SimplyTypedLambdaCalc where

import Control.Applicative
import Control.Monad
import Data.Char (isDigit)
import Data.Foldable
import Data.Maybe
import Text.Earley

--------------------------------------------------------------------------------
-- Term
--------------------------------------------------------------------------------

type Var
  = Int

data Term
  = TermVar Var
  | TermLam Type Term
  | TermApp Term Term
  | TermTrue
  | TermFalse
  | TermIf Term Term Term
  deriving Show

pattern Value :: Term -> Term
pattern Value t <- (matchValue -> Just t)

matchValue :: Term -> Maybe Term
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

type Context
  = [(Var, Type)]

--------------------------------------------------------------------------------
-- Type checking
--------------------------------------------------------------------------------

typeOf :: Term -> Maybe Type
typeOf =
  typeOf' []

typeOf' :: Context -> Term -> Maybe Type
typeOf' ctx = \case
  TermVar x ->
    Just (fromJust (lookup x ctx))
  TermLam y t -> do
    r <- typeOf' ((0, y) : ctx) t
    pure (TypeFun y r)
  TermApp t1 t2 -> do
    TypeFun y1 y2 <- typeOf' ctx t1
    z <- typeOf' ctx t2
    guard (y1 == z)
    pure y2
  TermTrue ->
    pure TypeBool
  TermFalse ->
    pure TypeBool
  TermIf t1 t2 t3 -> do
    TypeBool <- typeOf' ctx t1
    y2 <- typeOf' ctx t2
    y3 <- typeOf' ctx t3
    guard (y2 == y3)
    pure y2

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
  TermApp (TermLam _ t) (Value v) ->
    Just (shift (-1) (subst 0 (shift 1 v) t))
  TermApp (Value v1) t2 ->
    TermApp v1 <$> eval1 t2
  TermApp t1 t2 ->
    TermApp <$> eval1 t1 <*> pure t2
  TermLam{} ->
    Nothing
  TermVar{} ->
    Nothing
  TermTrue ->
    Nothing
  TermFalse ->
    Nothing
  TermIf TermTrue t _ ->
    Just t
  TermIf TermFalse _ t ->
    Just t
  TermIf t1 t2 t3 ->
    TermIf <$> eval1 t1 <*> pure t2 <*> pure t3

evalString :: [Char] -> Maybe Term
evalString =
  fmap eval . parseTerm

--------------------------------------------------------------------------------
-- Substitution
--------------------------------------------------------------------------------

-- | @subst x t s@ substitutes occurrences of @x@ in @s@ to @t@.
subst :: Var -> Term -> Term -> Term
subst x =
  go 0
 where
  go :: Int -> Term -> Term -> Term
  go c t = \case
    TermVar y ->
      if x+c == y
        then shift c t
        else TermVar y
    TermLam y s ->
      TermLam y (go (c+1) t s)
    TermApp t1 t2 ->
      TermApp (go c t t1) (go c t t2)
    TermTrue ->
      TermTrue
    TermFalse ->
      TermFalse
    TermIf t1 t2 t3->
      TermIf (go c t t1) (go c t t2) (go c t t3)

-- | @shift d t@ shifts all variables by @d@.
shift :: Int -> Term -> Term
shift d t =
  shift_ d 0 t

-- | @shift_ d c t@ shifts all variables @>= c@ by @d@.
shift_ :: Int -> Var -> Term -> Term
shift_ d c = \case
  TermVar v | v >= c ->
    TermVar (v+d)
  TermVar v ->
    TermVar v
  TermLam y t ->
    TermLam y (shift_ d (c+1) t)
  TermApp t1 t2 ->
    TermApp (shift_ d c t1) (shift_ d c t2)
  TermTrue ->
    TermTrue
  TermFalse ->
    TermFalse
  TermIf t1 t2 t3 ->
    TermIf (shift_ d c t1) (shift_ d c t2) (shift_ d c t3)

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

parseTerm :: [Char] -> Maybe Term
parseTerm bytes = do
  ([term], _) <- pure (fullParses stlcParser bytes)
  pure term

stlcParser :: Parser e [Char] Term
stlcParser =
  parser termGrammar

termGrammar :: forall r e. (Grammar r) (Prod r e Char Term)
termGrammar = mdo
  prodType :: Prod r e Char Type <-
    typeGrammar

  prodTerm :: Prod r e Char Term <-
    rule
      (asum
        [ prodAtom
        , do
            lexeme (token '\\')
            ty <- prodType
            lexeme (token '.')
            term <- prodTerm
            pure (TermLam ty term)
        , do
            term1 <- prodTerm
            term2 <- prodAtom
            pure (TermApp term1 term2)
        ])

  let prodAtom :: Prod r e Char Term
      prodAtom =
        asum
          [ TermTrue <$ lexeme (list "true")
          , TermFalse <$ lexeme (list "false")
          , TermVar . read <$> lexeme (some (satisfy isDigit))
          , do
              lexeme (token '(')
              term <- prodTerm
              lexeme (token ')')
              pure term
          ]

  pure prodTerm

typeGrammar :: forall r e. Grammar r (Prod r e Char Type)
typeGrammar = mdo
  prodType :: Prod r e Char Type <-
    rule
      (asum
        [ prodAtom
        , do
            t1 <- prodAtom
            lexeme (list "->")
            t2 <- prodType
            pure (TypeFun t1 t2)
        ])

  let prodAtom :: Prod r e Char Type
      prodAtom =
        asum
          [ TypeBool <$ lexeme (list "bool")
          , do
              lexeme (token '(')
              ty <- prodType
              lexeme (token ')')
              pure ty
          ]

  pure prodType

lexeme :: Prod r e Char a -> Prod r e Char a
lexeme = (<* many (token ' '))
