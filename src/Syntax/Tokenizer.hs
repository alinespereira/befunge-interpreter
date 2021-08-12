module Syntax.Tokenizer (parseInstruction) where

import           Syntax.Definitions
import           Syntax.Instruction (Instruction)

parseInstruction :: Char -> Instruction
parseInstruction c
  | isADigit c = getPushDigitInstruction c
  | isAnOperator c = getOperatorInstruction c
  | isAMove c = getMoveInstruction c
  | isACondition c = getConditionInstruction c
  | otherwise = getPushCharInstruction c
