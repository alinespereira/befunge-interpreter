module Interpreter.Program where

import           Data.Either        (isLeft)
import           Parser.Board
import           Syntax.Instruction

type AsciiCode = Int

type ProgramError = String

data StringMode = StringModeOn | StringModeOff deriving (Show, Eq)

data ProgramStatus
  = Initialized
  | Started
  | Stopped
  | Ended
  deriving (Show, Eq)

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
setProgramBoard board program = program { programBoard = board }

getCurrentInstruction :: Program -> Instruction
getCurrentInstruction program = getBoardValue i j board
  where
    (Cursor i j) = programCursor program
    board = programBoard program

setProgramDirection :: MoveDirection -> Program -> Program
setProgramDirection dir program
  | dir /= MoveRandom = program { programDirection = dir }
  | otherwise = undefined

setProgramCursor :: Cursor -> Program -> Program
setProgramCursor cursor program
  = program { programCursor = cursor }

toggleStringMode :: Program -> Program
toggleStringMode program =
  case stringMode program of
    StringModeOn  -> program { stringMode = StringModeOff }
    StringModeOff -> program { stringMode = StringModeOn }
