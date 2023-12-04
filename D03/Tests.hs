module Main (main) where

import D03
import Test.HUnit
  ( Counts (Counts),
    Test (TestCase, TestLabel, TestList),
    assertEqual,
    runTestTT,
  )

testInput =
  "467..114..\n\
  \...*......\n\
  \..35..633.\n\
  \......#...\n\
  \617*......\n\
  \.....+.58.\n\
  \..592.....\n\
  \......755.\n\
  \...$.*....\n\
  \.664.598..\n"

test1 = TestCase (assertEqual "task1" 4361 (task1 testInput))

test2 = TestCase (assertEqual "task2" 467835 (task2 testInput))

tests = TestList [TestLabel "task1" test1, TestLabel "task2" test2]

main :: IO Counts
main = runTestTT tests
