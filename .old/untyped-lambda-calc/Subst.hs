module Subst where

import Term

-- | Substitute all occurences of a variable with a term.
data Subst = Subst Var Term

-- | Transform a substitution per moving under a lambda. The original De
-- Bruijn index and free variables must be incremented to account for the
-- new lambda.
under :: Subst -> Subst
under (Subst v s) = Subst (v+1) (shift 1 s)

-- @subst x s t@ substitutes all free occurrences of @x@ in @t@ with @s@.
--
-- [x -> s]t
subst :: Subst -> Term -> Term
subst s0@(Subst x s) = \case
  Var v
    | x == v    -> s
    | otherwise -> Var v
  Lam n t -> Lam n (subst (under s0) t)
  App t1 t2 -> App (subst s0 t1) (subst s0 t2)
