module Main where

import Checker
import qualified Data.Map as Map
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Literals" $
    do
      let ctx = Map.empty
      it "should check bool literals" $
        infer ctx BoolLiteral `shouldBe` Just Bool
      it "should check int literals" $
        infer ctx IntLiteral `shouldBe` Just Int

  describe "Conditionals" $
    do
      let ctx = Map.empty
      it "should accept a correct conditional" $
        infer ctx (Conditional (BoolLiteral, IntLiteral, IntLiteral)) `shouldBe` Just Int
      it "should reject conditional with non-bool condition" $
        infer ctx (Conditional (IntLiteral, IntLiteral, IntLiteral)) `shouldBe` Nothing

  describe "Functions" $
    do
      let negation = Lambda ((Bool, Bool), "x", Conditional (Variable "x", BoolLiteral, BoolLiteral))
      let toBinary = Lambda ((Bool, Int), "x", Conditional (Variable "x", IntLiteral, IntLiteral))
      do
        let ctx = Map.empty
        it "should typecheck x -> if x then 1 else 0" $
          infer ctx toBinary `shouldBe` Just (Function (Bool, Int))
        it "should typecheck x -> if x then false else true" $
          infer ctx negation `shouldBe` Just (Function (Bool, Bool))
