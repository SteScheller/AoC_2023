module Main (main) where

import DXX (task1, task2)
import System.IO ()

main :: IO ()
main = do
  iData <- readFile "DXX/input.txt"
  print (task1 iData)
  print (task2 iData)