module Syntax.TokenizerSpec where

import           Syntax.Instruction
import           Syntax.Tokenizer
import           Test.Hspec

spec :: Spec
spec = do
  describe "Tokenizer" $ do
    parseDigit
    parseOperation

parseDigit :: Spec
parseDigit = do
  describe "parseInstruction" $ do
    it "returns just a push digit instruction" $ do
      parseInstruction '0' `shouldBe` PushDigit 0
      parseInstruction '1' `shouldBe` PushDigit 1
      parseInstruction '2' `shouldBe` PushDigit 2
      parseInstruction '3' `shouldBe` PushDigit 3
      parseInstruction '4' `shouldBe` PushDigit 4
      parseInstruction '5' `shouldBe` PushDigit 5
      parseInstruction '6' `shouldBe` PushDigit 6
      parseInstruction '7' `shouldBe` PushDigit 7
      parseInstruction '8' `shouldBe` PushDigit 8
      parseInstruction '9' `shouldBe` PushDigit 9

parseOperation :: Spec
parseOperation = do
  describe "parseInstruction" $ do
    it "returns just an operation instruction" $ do
      parseInstruction '+' `shouldBe` Add
      parseInstruction '-' `shouldBe` Sub
      parseInstruction '*' `shouldBe` Mult
      parseInstruction '/' `shouldBe` Div
      parseInstruction '%' `shouldBe` Modulo
      parseInstruction '!' `shouldBe` Not
      parseInstruction '`' `shouldBe` GreaterThan
