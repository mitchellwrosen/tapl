{-# language OverloadedStrings #-}

module Main where

import Stlc.Eval
import Stlc.Parser
import Stlc.TypeCheck

import Bound
import Data.Foldable (toList)
import System.Exit

main :: IO ()
main = do
  let input =
        -- "case t of { <bar = y> => true; <foo = x> => false; }"
        "let \
        \  t = <foo=true> as <foo:bool, bar:unit> \
        \in \
        \  case t of { \
        \    <foo = x> => x;\
        \    <bar = y> => true;\
        \  }"

  putStrLn "-- PARSE --"
  term <-
    case parseTerm input of
      Left e -> do
        print e
        exitFailure
      Right t ->
        pure t
  print term

  putStrLn "\n-- TYPE CHECK --"
  case closed term of
    Nothing -> do
      putStrLn "Free variables!"
      print (toList term)
    Just term' ->
      case typeOf term' of
        Nothing -> do
          putStrLn "Type error!"
          exitFailure
        Just ty ->
          print ty

  putStrLn "\n-- EVAL --"
  print (eval term)
