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
      parseInstruction '0' `shouldBe` Push (NumVal 0)
      parseInstruction '1' `shouldBe` Push (NumVal 1)
      parseInstruction '2' `shouldBe` Push (NumVal 2)
      parseInstruction '3' `shouldBe` Push (NumVal 3)
      parseInstruction '4' `shouldBe` Push (NumVal 4)
      parseInstruction '5' `shouldBe` Push (NumVal 5)
      parseInstruction '6' `shouldBe` Push (NumVal 6)
      parseInstruction '7' `shouldBe` Push (NumVal 7)
      parseInstruction '8' `shouldBe` Push (NumVal 8)
      parseInstruction '9' `shouldBe` Push (NumVal 9)

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
