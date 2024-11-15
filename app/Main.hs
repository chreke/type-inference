module Main where

import qualified Data.Map as Map
import Checker

literal :: Term
literal = IntLiteral

negation :: Term
negation = Conditional (BoolLiteral, BoolLiteral, BoolLiteral)

main :: IO ()
main = do
  let ctx = Map.empty
  let state = Checker.initState
  print (Checker.infer state ctx literal)
  print (Checker.infer state ctx negation)
