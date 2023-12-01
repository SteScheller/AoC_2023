import Data.Char (isDigit)
import qualified Data.Map as Map
import System.IO ()

firstLast :: [a] -> [a]
firstLast x = [head x, last x]

task1 :: String -> Int
task1 x = sum (map (read . firstLast . filter isDigit) $ lines x :: [Int])

type StringMap = Map.Map String String

replaceWithMap :: StringMap -> String -> String
replaceWithMap m x = 
    if null m then x
    else let (old, new) = Map.findMin m
             -- TODO: implement replace with isInfixOf x' = replace old new x
             x' = x
         in replaceWithMap (Map.deleteMin m) x'

task2 :: String -> Int
task2 x = sum (map (read . firstLast . filter isDigit . replaceWithMap digitMap) $ lines x :: [Int])
  where
    digitMap =
      Map.fromList
        [ ("one", "1"),
          ("two", "2"),
          ("three", "3"),
          ("four", "4"),
          ("five", "5"),
          ("six", "6"),
          ("seven", "7"),
          ("eight", "8"),
          ("nine", "9")
        ]

main :: IO ()
main = do
  iData <- readFile "input.txt"
  print (task1 iData)
  print (task2 iData)