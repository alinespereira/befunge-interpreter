module Interpreter.ProgramSpec where

import           Interpreter.Program
import           Test.Hspec

spec :: Spec
spec = do
  describe "Program" $ do
    parseToggleStringMode

filePath :: String
filePath = "/Users/diego.pereira/Documents/Estudos/haskell/befunge/resources/code.txt"

program :: IO Program
program =
  let code = lines <$> readFile filePath
  in initProgram 16 25 <$> code

parseToggleStringMode :: Spec
parseToggleStringMode = do
  describe "toggleStringMode" $ do
    it "should initialize stringMode to StringModeOff" $ do
      mode <- stringMode <$> program
      mode `shouldBe` StringModeOff
    it "should toggle stringMode to StringModeOn" $ do
      mode <- stringMode . toggleStringMode <$> program
      mode `shouldBe` StringModeOn
    it "should toggle stringMode back to to StringModeOff" $ do
      mode <- stringMode . toggleStringMode . toggleStringMode <$> program
      mode  `shouldBe` StringModeOff


