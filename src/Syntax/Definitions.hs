module Syntax.Definitions
  ( isADigit,
    getPushDigitInstruction,
    getPushCharInstruction,
    isAnOperator,
    getOperatorInstruction,
    isAMove,
    getMoveInstruction,
    isACondition,
    getConditionInstruction,
  )
where

import           Data.Char          (digitToInt, isDigit)
import           Data.List          (elemIndex)
import           Syntax.Instruction

type InstructionDefinition = (Char, Instruction)

isA :: [InstructionDefinition] -> Char -> Bool
isA defs = flip elem (map fst defs)

getDefsInstruction :: [InstructionDefinition] -> Char -> Instruction
getDefsInstruction defs c =
  case elemIndex c (map fst defs) of
    Just idx -> map snd defs !! idx
    Nothing  -> error "Not an instruction"

isADigit :: Char -> Bool
isADigit = isDigit

getPushDigitInstruction :: Char -> Instruction
getPushDigitInstruction c = Push (NumVal (digitToInt c))

getPushCharInstruction :: Char -> Instruction
getPushCharInstruction c = Push (CharVal c)

operator :: [InstructionDefinition]
operator =
  [ ('+', Add),
    ('-', Sub),
    ('*', Mult),
    ('/', Div),
    ('%', Modulo),
    ('!', Not),
    ('`', GreaterThan)
  ]

isAnOperator :: Char -> Bool
isAnOperator = isA operator

getOperatorInstruction :: Char -> Instruction
getOperatorInstruction = getDefsInstruction operator

move :: [InstructionDefinition]
move =
  [ ('<', Move MoveLeft),
    ('>', Move MoveRight),
    ('^', Move MoveUp),
    ('v', Move MoveDown),
    ('?', Move MoveRandom)
  ]

isAMove :: Char -> Bool
isAMove = isA move

getMoveInstruction :: Char -> Instruction
getMoveInstruction = getDefsInstruction move

condition :: [InstructionDefinition]
condition =
  [ ('_', If HorizontalIf),
    ('|', If VerticalIf)
  ]

isACondition :: Char -> Bool
isACondition = isA condition

getConditionInstruction :: Char -> Instruction
getConditionInstruction = getDefsInstruction condition
