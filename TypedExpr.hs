module TypedExpr where

import Control.Monad

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

--------------------------------------------------------------------------------
-- Type
-------------------------------------------------------------------------------

data Type
  = TypeBool
  | TypeNat
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Type checking
-------------------------------------------------------------------------------

typeOf :: Term -> Maybe Type
typeOf = \case
  TermTrue ->
    Just TypeBool
  TermFalse ->
    Just TypeBool
  TermIf t1 t2 t3 -> do
    TypeBool <- typeOf t1
    y2 <- typeOf t2
    y3 <- typeOf t3
    guard (y2 == y3)
    Just y2
  TermZero ->
    Just TypeNat
  TermSucc t -> do
    TypeNat <- typeOf t
    Just TypeNat
  TermPred t -> do
    TypeNat <- typeOf t
    Just TypeNat
  TermIsZero t -> do
    TypeNat <- typeOf t
    Just TypeBool
