module Main where

import           Interpreter.Program (initProgram)
import           Parser.Board

filePath :: String
filePath = "/Users/diego.pereira/Documents/Estudos/haskell/befunge/resources/code.txt"

main :: IO ()
main = do
  putStrLn "Type the path to your program: "
  print . initProgram 16 25 . lines <$> (readFile =<< getLine)
  -- let board = readBoard 25 80 <$> lines code
  return ()
