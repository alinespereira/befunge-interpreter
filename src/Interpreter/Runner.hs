module Interpreter.Runner where

import           Data.Either             (fromLeft)
import           Interpreter.Instruction
import           Interpreter.Program

run :: Program -> Program
run program | programFailed program = error (fromLeft "Program failed" (programStatus program))
            | programEnded program = program
            | otherwise = let newProgram = process program
                          in run newProgram
