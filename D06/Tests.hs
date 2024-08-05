module Main (main) where

import D06
import Test.HUnit
  ( Counts (Counts),
    Test (TestCase, TestLabel, TestList),
    assertEqual,
    runTestTT,
  )

testInput =
  "Time:      7  15   30\n\
  \Distance:  9  40  200\n"

test1 = TestCase (assertEqual "task1" 288 (task1 testInput))

test2 = TestCase (assertEqual "task2" 467835 (task2 testInput))

tests = TestList [TestLabel "task1" test1, TestLabel "task2" test2]

main :: IO Counts
main = runTestTT tests
