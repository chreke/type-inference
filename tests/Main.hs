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

  describe "Conditionals" $
    do
      it "should accept a correct conditional" $
        infer (Conditional (BoolLiteral, IntLiteral, IntLiteral)) `shouldBe` Just Int
      it "should reject conditional with non-bool condition" $
        infer (Conditional (IntLiteral, IntLiteral, IntLiteral)) `shouldBe` Nothing
