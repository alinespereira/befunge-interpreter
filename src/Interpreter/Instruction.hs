module Interpreter.Instruction (process) where

import Interpreter.Program
import Parser.Board
import Syntax.Instruction

shape :: [[a]] -> (Int, Int)
shape arr = (length arr, (length . head) arr)

process :: Program -> Program
process program = undefined
  where
    board = programBoard program
    cur@(Cursor i j) = programCursor program
    mode = stringMode program
    status = programStatus program
    stack = programStack program
    lastIn = lastInstruction program
    direction = programDirection program
    currentIn = getBoardValue i j board
