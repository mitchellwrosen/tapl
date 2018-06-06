module Eval where

import Subst
import Term

-- Small-step call-by-value evaluation. Nothing indicates the value cannot
-- be evaluated an further, as with a variable, lambda, or wrongly typed
-- application.
eval :: Term -> Maybe Term
eval = \case
  App (Lam _ t) v | isval v ->
    pure (shift (-1) (subst (Subst 0 (shift 1 v)) t))
  App v t2 | isval v -> do
    t2' <- eval t2
    pure (App v t2')
  App t1 t2 -> do
    t1' <- eval t1
    pure (App t1' t2)
  _ -> Nothing

-- Big-step call-by-value evaluation.
eval' :: Term -> Term
eval' t0 = maybe t0 eval' (eval t0)
