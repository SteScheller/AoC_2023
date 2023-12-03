module Main (main) where

import D01
import System.IO ()

main :: IO ()
main = do
  iData <- readFile "D01/input.txt"
  print (task1 iData)
  print (task2 iData)