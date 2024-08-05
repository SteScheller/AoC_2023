module Main (main) where

import D06
import Test.HUnit
  ( Counts (Counts),
    Test (TestCase, TestLabel, TestList),
    assertEqual,
    runTestTT,
  )

testInput1 =
  "Time:      7  15   30\n\
  \Distance:  9  40  200\n"

testInput2 =
  "Time:      71530\n\
  \Distance:  940200\n"

test1 = TestCase (assertEqual "task1" 288 (task1 testInput1))

test2 = TestCase (assertEqual "task2" 71503 (task2 testInput2))

tests = TestList [TestLabel "task1" test1, TestLabel "task2" test2]

main :: IO Counts
main = runTestTT tests
