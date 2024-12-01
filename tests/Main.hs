module Main where

import Checker
import qualified Data.Map as Map
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Literals" $
