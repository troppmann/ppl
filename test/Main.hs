module Main where

import ParserTest
import SampleTest
import Test.Tasty

tests :: TestTree
tests = testGroup "Tests" [ParserTest.tests, SampleTest.tests]

main :: IO ()
main = defaultMain Main.tests
