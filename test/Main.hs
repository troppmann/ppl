module Main where

import ApproximateIntegrationTest
import DimProbTest
import InferTest
import MapTest
import ParserTest
import QueryTest
import OptimizeTest
import SampleTest
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ ParserTest.tests,
      SampleTest.tests,
      ApproximateIntegrationTest.tests,
      QueryTest.tests,
      DimProbTest.tests,
      MapTest.tests,
      InferTest.tests,
      OptimizeTest.tests
    ]

main :: IO ()
main = defaultMain Main.tests
