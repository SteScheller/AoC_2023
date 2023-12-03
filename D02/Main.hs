module Main (main) where

import D02
import System.IO ()

main :: IO ()
main = do
  iData <- readFile "D02/input.txt"
  print (task1 iData)
  print (task2 iData)