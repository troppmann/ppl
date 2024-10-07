module Main where

import ApproximateIntegrationTest
import InterpretTest
import ParserTest
import SampleTest
import Test.Tasty

tests :: TestTree
tests = testGroup "Tests" [ParserTest.tests, SampleTest.tests, InterpretTest.tests, ApproximateIntegrationTest.tests]

main :: IO ()
main = defaultMain Main.tests
