module Interpreter.Instruction (process, moveCursor) where

import           Interpreter.Program
import           Parser.Board
import           Syntax.Instruction

shape :: [[a]] -> (Int, Int)
shape arr = (length arr, (length . head) arr)

process :: Program -> Program
process program =
  case currentIn of
    Move dir -> setProgramDirection dir program
    _        -> program
  where
    board = programBoard program
    cur@(Cursor i j) = programCursor program
    mode = stringMode program
    status = programStatus program
    stack = programStack program
    lastIn = lastInstruction program
    direction = programDirection program
    currentIn = getCurrentInstruction program

moveCursor :: Program -> Program
moveCursor program = setProgramCursor cur program
  where
    dir = programDirection program
    Cursor i j = programCursor program
    cur = case dir of
      MoveLeft   -> Cursor i (j - 1)
      MoveRight  -> Cursor i (j + 1)
      MoveUp     -> Cursor (i - 1) j
      MoveDown   -> Cursor (i + 1) j
      MoveRandom -> Cursor i (j - 1)
