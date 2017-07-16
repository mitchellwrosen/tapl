module GenParserLib
  ( perror
  , var
  , true
  , false
  , lam
  , app
  ) where

import Term
import Token
import Type (Type)

perror :: [Token] -> a
perror = error . show

var :: String -> Term
var = Var

true :: Term
true = Bool True

false :: Term
false = Bool False

lam :: String -> Type -> Term -> Term
lam = Lam

app :: Term -> Term -> Term
app = App
