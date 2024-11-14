module Main where

import Checker
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Literals" $
    do
      it "should check bool literals" $
        infer BoolLiteral `shouldBe` Just Bool
      it "should check int literals" $
        infer IntLiteral `shouldBe` Just Int
