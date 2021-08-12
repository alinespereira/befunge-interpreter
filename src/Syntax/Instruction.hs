module Syntax.Instruction where

data MoveDirection
  = MoveRight
  | MoveLeft
  | MoveUp
  | MoveDown
  | MoveRandom
  deriving (Show, Eq)

data IfDirection
  = HorizontalIf
  | VerticalIf
  deriving (Show, Eq)

data Value
  = NumVal Int
  | CharVal Char
  deriving (Show, Eq)

data Instruction
  = Push Value
  | Add
  | Sub
  | Mult
  | Div
  | Modulo
  | Not
  | GreaterThan
  | Move MoveDirection
  | If IfDirection
  | StringMode
  | Duplicate
  | Swap
  | PopDiscard
  | PopInt
  | PopChar
  | Bridge
  | Put
  | Get
  | AskNumber
  | AskChar
  | End
  | NoOp
  deriving (Show, Eq)
