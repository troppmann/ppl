module Main where

import InterpretTest
import ParserTest
import SampleTest
import Test.Tasty

tests :: TestTree
tests = testGroup "Tests" [ParserTest.tests, SampleTest.tests, InterpretTest.tests]

main :: IO ()
main = defaultMain Main.tests
