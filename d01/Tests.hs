module Main (main) where

import D01
import Test.HUnit
  ( Counts (Counts),
    Test (TestCase, TestLabel, TestList),
    assertEqual,
    runTestTT,
  )

test1 = TestCase (assertEqual "task1" 142 (task1 "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"))

test2 = TestCase (assertEqual "task2" 281 (task2 "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"))

tests = TestList [TestLabel "task1" test1, TestLabel "task2" test2]

main :: IO Counts
main = runTestTT tests