module Stlc.Eval where

import Stlc.Parser (parseTerm)
import Stlc.Term
import Stlc.TypeCheck (typeOf)

import Bound
import Data.Maybe
import Data.Foldable (toList)
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
  TermAs t y ->
    TermAs <$> eval1 t <*> pure y
  TermTuple ts -> do
    (xs, t:ys) <- pure (span isValue ts)
    v <- eval1 t
    pure (TermTuple (xs ++ [v] ++ ys))
  TermTupleIx (Value (TermTuple ts)) i ->
    Just (ts !! (i-1))
  TermTupleIx t i ->
    TermTupleIx <$> eval1 t <*> pure i
  TermRecord ts -> do
    (xs, (l,t):ys) <- pure (span (isValue . snd) ts)
    v <- eval1 t
    pure (TermRecord (xs ++ [(l,v)] ++ ys))
  TermRecordIx (Value (TermRecord ts)) l ->
    Just (fromJust (lookup l ts))
  TermRecordIx t l ->
    TermRecordIx <$> eval1 t <*> pure l
  TermVariant l t y ->
    TermVariant <$> pure l <*> eval1 t <*> pure y
  TermCase (TermVariant l (Value t) _) us ->
    Just (instantiate1 t (fromJust (lookup l (toList us))))
  TermCase t us ->
    TermCase <$> eval1 t <*> pure us
  TermIf TermTrue t _ ->
    Just t
  TermIf TermFalse _ t ->
    Just t
  TermIf t1 t2 t3 ->
    TermIf <$> eval1 t1 <*> pure t2 <*> pure t3

  TermVar{} -> Nothing
  TermLam{} -> Nothing
  TermUnit -> Nothing
  TermTrue -> Nothing
  TermFalse -> Nothing

{-
evalString :: [Char] -> Maybe (Term Void)
evalString =
  f . parseTerm . Latin1.pack
 where
  f :: Term [Char] -> Maybe (Term Void)
  f t = do
    t' <- closed t
    _ <- typeOf t'
    pure (eval t')
-}
