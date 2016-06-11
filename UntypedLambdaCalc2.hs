{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module UntypedLambdaCalc2 where

-- | Untyped lambda calculus, using De Bruijn indices.

import Data.Monoid
import Data.Set       (Set, (\\))
import Data.Text.Lazy (Text)
import Prelude        hiding (and, fst, not, or, snd, succ)

import qualified Data.Set                     as Set
import qualified Text.PrettyPrint.Leijen.Text as PP

-- t ::=      -- terms:
--       x    -- variable
--       \x.t -- abstraction
--       t t  -- application
--
-- v ::=      -- values:
--       \x.t -- abstraction value


data Term
  = Var Int
  | Lam Text Term
  | App Term Term
  deriving (Show)
infixl 5 `App`

ppTerm :: Term -> PP.Doc
ppTerm = go [] mempty
 where
  go :: [Text] -> Set Text -> Term -> PP.Doc
  go ctx ns = \case
    Var n -> PP.text (ix n ctx)
    Lam n t ->
      let
        n' = freshify n
      in
        PP.char 'λ'
          PP.<> PP.text n'
          PP.<> PP.char '.'
          PP.<+> go (n':ctx) (Set.insert n' ns) t
    App t1 t2 ->
      let
        parens1 = case t1 of
          Lam _ _ -> PP.parens
          _       -> id

        parens2 = case t2 of
          Var _ -> PP.parens
          _     -> id
      in
        parens1 (go ctx ns t1) PP.<+> parens2 (go ctx ns t2)

   where
    ix :: Int -> [Text] -> Text
    ix _ []      = "<invalid index>"
    ix 0 (x:_)   = x
    ix !n (_:xs) = ix (n-1) xs

    freshify :: Text -> Text
    freshify n
      | n `Set.member` ns = freshify (n <> "'")
      | otherwise = n



data Subst = !Int :-> Term
infixl 5 :->

-- @fvs t@ finds all free variables in @t@.
fvs :: Term -> Set Int
fvs = go 0
 where
  go :: Int -> Term -> Set Int
  go !c = \case
    Var v
      | v >= c    -> Set.singleton v
      | otherwise -> mempty
    Lam _ t -> go (c+1) t
    App t1 t2 -> go c t1 <> go c t2

-- @shift d t@ shifts all free variables in @t@ by @d@.
shift :: Int -> Term -> Term
shift d = go 0
 where
  go :: Int -> Term -> Term
  go !c = \case
    Var k
      | k < c     -> Var k
      | otherwise ->
          let
            !k' = k + d
          in
            Var k'
    Lam n t ->
      let
        !t' = go (c+1) t
      in
        Lam n t'
    App t1 t2 ->
      let
        !t1' = go c t1
        !t2' = go c t2
      in
        App t1' t2'

-- @subst x s t@ substitutes all free occurrences of @x@ in @t@ with @s@.
--
-- [x -> s]t
subst :: Subst -> Term -> Term
subst (x :-> s) = \case
  Var v
    | x == v    -> s
    | otherwise -> Var v
  Lam n t ->
    let
      !s' = shift 1 s
      !t' = subst (x+1 :-> s') t
    in
      Lam n t'
  App t1 t2 ->
    let
      !t1' = subst (x :-> s) t1
      !t2' = subst (x :-> s) t2
    in
      App t1' t2'

-- Small-step call-by-value evaluation
eval :: Term -> Maybe Term
eval = \case
  -- E-AppAbs
  App (Lam _ t) v@(Lam _ _) ->
    pure (shift (-1) (subst (0 :-> shift 1 v) t))
  -- E-App2
  App v@(Lam _ _) t2 -> do
    !t2' <- eval t2
    pure (App v t2')
  -- E-App1
  App t1 t2 -> do
    !t1' <- eval t1
    pure (App t1' t2)
  _ -> Nothing

-- Big-step call-by-value evaluation
eval' :: Term -> Term
eval' t0 = maybe t0 eval' (eval t0)
