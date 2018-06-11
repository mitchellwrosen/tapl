module Stlc.Eval where

import Stlc.Parser (parseTerm)
import Stlc.Term
import Stlc.TypeCheck (typeOf)

import Bound
import Data.Void

import qualified Data.ByteString.Char8 as Latin1

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
  TermLet (Value t) s ->
    Just (instantiate1 t s)
  TermLet t s ->
    TermLet <$> eval1 t <*> pure s
  TermAs (Value t) _ ->
    Just t
  TermIf TermTrue t _ ->
    Just t
  TermIf TermFalse _ t ->
    Just t
  TermIf t1 t2 t3 ->
    TermIf <$> eval1 t1 <*> pure t2 <*> pure t3

  TermVar{} -> Nothing
  TermLam{} -> Nothing
  TermAs{} -> Nothing
  TermUnit -> Nothing
  TermTrue -> Nothing
  TermFalse -> Nothing

evalString :: [Char] -> Maybe (Term Void)
evalString =
  f . parseTerm . Latin1.pack
 where
  f :: Term [Char] -> Maybe (Term Void)
  f t = do
    t' <- closed t
    _ <- typeOf t'
    pure (eval t')
