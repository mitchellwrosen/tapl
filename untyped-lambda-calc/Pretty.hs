{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty where

import Term

import Data.Monoid
import Data.Set                     (Set)
import Data.Text.Lazy               (Text)
import Text.PrettyPrint.Leijen.Text hiding ((<>))

import qualified Data.Set as Set

instance Pretty Term where
  pretty = go [] mempty
   where
    go :: [Text] -> Set Text -> Term -> Doc
    go ctx ns = \case
      Var n -> text (ix n ctx)
      Lam n t ->
        char 'λ'
          <>  text n'
          <>  char '.'
          <+> go (n':ctx) (Set.insert n' ns) t
       where
        n' = freshify n
      App t1 t2 ->
        let
          parens1 = case t1 of
            Lam _ _ -> parens
            _       -> id

          parens2 = case t2 of
            Var _ -> id
            _     -> parens
        in
          parens1 (go ctx ns t1) <+> parens2 (go ctx ns t2)

     where
      ix :: Int -> [Text] -> Text
      ix _ []     = error "invalid index"
      ix 0 (x:_)  = x
      ix n (_:xs) = ix (n-1) xs

      freshify :: Text -> Text
      freshify n
        | n `Set.member` ns = freshify (n <> "'")
        | otherwise = n
