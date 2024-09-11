module Main where

import SampleTest
import Test.Tasty

tests :: TestTree
tests = testGroup "Tests" [SampleTest.tests]

main :: IO ()
main = defaultMain Main.tests
