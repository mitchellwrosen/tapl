-- Horrible pretty-printing functions for debugging. They don't work properly.

module Stlc.Pretty where

import Stlc.Term
import Stlc.Type

import Bound

import qualified Data.Text.Prettyprint.Doc as Ppr
import qualified Data.Text.Prettyprint.Doc.Render.Text as Ppr

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
  TermAs t y -> pprTerm t <> " as " <> pprType y
  TermUnit -> "unit"
  TermTrue -> "true"
  TermFalse -> "false"
  TermIf t1 t2 t3 ->
    "if " <> pprTerm t1 <> " then " <> pprTerm t2 <> " else " <> pprTerm t3

pprType :: Type -> Ppr.Doc ()
pprType = \case
  TypeBool -> "bool"
  TypeUnit -> "unit"
  TypeFun t1 t2 -> pprType t1 <> " -> " <> pprType t2

