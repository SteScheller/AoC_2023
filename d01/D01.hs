module D01(task1, task2) where

import Data.Char (isDigit)
import Data.List.Extra
import Data.Map qualified as Map

firstLast :: [a] -> [a]
firstLast x = [head x, last x]

task1 :: String -> Int
task1 x = sum (map (read . firstLast . filter isDigit) $ lines x :: [Int])

task2 :: String -> Int
task2 x = sum (map (read . firstLast . filter isDigit . (\x -> replaceFront dm dm x ++ replaceBack dm dm x)) $ lines x :: [Int])
  where
    dm =
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
    replaceFront _ _ [] = []
    replaceFront mo mr x =
      if null mr
        then head x : replaceFront mo mo (tail x)
        else
          let (old, new) = Map.findMin mr
           in case stripPrefix old x of
                Just remainder -> new ++ remainder
                Nothing -> replaceFront mo (Map.deleteMin mr) x
    replaceBack _ _ [] = []
    replaceBack mo mr x =
      if null mr
        then replaceBack mo mo (init x) ++ [last x]
        else
          let (old, new) = Map.findMin mr
           in case stripSuffix old x of
                Just remainder -> remainder ++ new
                Nothing -> replaceBack mo (Map.deleteMin mr) x