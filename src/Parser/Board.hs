module Parser.Board
  ( readBoard,
    setBoardValue,
    getBoardValue,
    Board
  ) where

import           Syntax.Instruction (Instruction (NoOp))
import           Syntax.Tokenizer   (parseInstruction)


type Board = [[Instruction]]

makeRow :: Int -> a -> [a]
makeRow = replicate

parseExpandRow :: Int -> [Char] -> [Instruction]
parseExpandRow cols row =
  let expanded = map parseInstruction row ++ repeat NoOp
  in take cols expanded

readBoard :: Int -> Int -> [[Char]] -> Board
readBoard rows cols code =
  let
    parsedRows = map (parseExpandRow cols) code
    emptyRows = repeat (makeRow cols NoOp)
    totalRows = parsedRows ++ emptyRows
  in take rows totalRows

setBoardValue :: Int -> Int -> Char -> Board -> Board
setBoardValue i j c board =
  let
    instruction = parseInstruction c
    rowsBefore = take i board
    (currentRow:rowsAfter) = drop i board
    newCurrentRow = [if idx /= j then val else instruction
                    | (idx, val) <- zip [0..] currentRow]
  in rowsBefore ++ [newCurrentRow] ++ rowsAfter

getBoardValue :: Int -> Int -> Board -> Instruction
getBoardValue i j board = board !! i !! j
