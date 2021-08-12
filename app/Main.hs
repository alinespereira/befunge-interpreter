module Main where

import           Interpreter.Program (initProgram)
import           Parser.Board

filePath :: String
filePath = "/Users/diego.pereira/Documents/Estudos/haskell/befunge/resources/code.txt"

main :: IO ()
main = do
  code <- lines <$> readFile filePath
  let prog = initProgram 16 25 code
  print prog
  -- let board = readBoard 25 80 <$> lines code
  return ()
