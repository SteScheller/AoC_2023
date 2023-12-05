module Main (main) where

import D04 (task1, task2)
import System.IO ()

main :: IO ()
main = do
  iData <- readFile "D04/input.txt"
  print (task1 iData)
  print (task2 iData)