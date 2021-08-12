module Interpreter.Program where

import           Data.Either        (isLeft)
import           Parser.Board
import           Syntax.Instruction

type AsciiCode = Int

type ProgramError = String

data StringMode = StringModeOn | StringModeOff deriving (Eq)

data ProgramStatus
  = Initialized
  | Started
  | Stopped
  | Ended
  deriving (Eq)

data Cursor = Cursor
  { row :: Int,
    col :: Int
  }
  deriving (Eq)

instance Show Cursor where
  show (Cursor row col) = "(" ++ show row ++ ", " ++ show col ++ ")"

data Program = Program
  { programBoard     :: Board,
    programCursor    :: Cursor,
    stringMode       :: StringMode,
    programStatus    :: Either ProgramError ProgramStatus,
    programStack     :: [AsciiCode],
    lastInstruction  :: Maybe Instruction,
    programDirection :: MoveDirection
  }

instance Show Program where
  show (Program board _ _ _ _ _ _)= unlines $ map (unwords . map show) board

initProgram :: Int -> Int -> [[Char]] -> Program
initProgram rows cols code =
  Program { programBoard = readBoard rows cols code,
            programCursor = Cursor 0 0,
            stringMode = StringModeOff,
            programStatus = Right Initialized,
            programStack = [],
            lastInstruction = Nothing,
            programDirection = MoveRight
          }

programFailed :: Program -> Bool
programFailed program = either (const True) (const False) status
  where
    status = programStatus program

programEnded :: Program -> Bool
programEnded = isLeft . programStatus

setProgramBoard :: Board -> Program -> Program
setProgramBoard board (Program _ cursor mode status stack instruction direction) =
  Program board cursor mode status stack instruction direction

getCurrentInstruction :: Program -> Instruction
getCurrentInstruction program = getBoardValue i j board
  where
    (Cursor i j) = programCursor program
    board = programBoard program

setProgramDirection :: MoveDirection -> Program -> Program
setProgramDirection dir (Program board cursor mode status stack instruction _)
  | dir /= MoveRandom = Program board cursor mode status stack instruction dir
  | otherwise = undefined

setProgramCursor :: Cursor -> Program -> Program
setProgramCursor cursor (Program board _ mode status stack instruction direction)
  = Program board cursor mode status stack instruction direction

toggleStringMode :: Program -> Program
toggleStringMode (Program board cursor mode status stack instruction direction)
  | mode == StringModeOn
  = Program board cursor StringModeOff status stack instruction direction
  | otherwise
  = Program board cursor StringModeOn status stack instruction direction
