module Main (main) where

import D02
import Test.HUnit
  ( Counts (Counts),
    Test (TestCase, TestLabel, TestList),
    assertEqual,
    runTestTT,
    showCounts,
  )

testInput =
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
  \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
  \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
  \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
  \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n"

test1 = TestCase (assertEqual "task1" 8 (task1 testInput))

test2 = TestCase (assertEqual "task2" 2286 (task2 testInput))

tests = TestList [TestLabel "task1" test1, TestLabel "task2" test2]

main :: IO Counts
main = runTestTT tests
