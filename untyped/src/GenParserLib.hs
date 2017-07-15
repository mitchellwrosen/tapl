module GenParserLib
  ( perror
  , onVar
  , onLam
  , onApp
  , module Token
  ) where

import Term
import Token

import Bound (abstract1)

perror :: [Token] -> a
perror = error . show

onVar :: String -> Term String
onVar = Var

onLam :: String -> Term String -> Term String
onLam x t = Lam (abstract1 x t)

onApp :: Term a -> Term a -> Term a
onApp = App
