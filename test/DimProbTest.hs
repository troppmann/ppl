module DimProbTest
  ( tests,
  )
where

import Assert
import Representation
import Shorter
import Test.HUnit.Approx
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

testDimProbWithName :: TestName -> DimensionalProbability -> DimensionalProbability -> TestTree
testDimProbWithName testName (dim, prob) expectedDimProb@(expectedDim, expectedProb) = testCase msg $ do
  dim @?= expectedDim
  assertApproxEqual "" defaultErrorMargin expectedProb prob
  where
    msg = testName <> ":" <> show expectedDimProb

tests =
  testGroup
    "DimProbMath"
    [ testDimProbWithName "(0, 0.5 + 0.5)" (0, 0.5 + 0.5) (0, 1.0),
      testDimProbWithName "(0, 0.3) #+# (0, 0.1)" ((0, 0.3) #+# (0, 0.1)) (0, 0.4),
      testDimProbWithName "(0, 0.3) #+# (1, 0.23)" ((0, 0.3) #+# (1, 0.23)) (0, 0.3),
      testDimProbWithName "(0, 0.3) #+# (10, 0.23)" ((0, 0.3) #+# (10, 0.23)) (0, 0.3),
      testDimProbWithName "(10, 0.3) #+# (3, 0.23)" ((10, 0.3) #+# (3, 0.23)) (3, 0.23),
      testDimProbWithName "(3, 0.3) #+# (0, 0.0)" ((10, 0.3) #+# (3, 0.23)) (3, 0.23),
      testDimProbWithName "(90, 0.0) #+# (100, 0.0)" ((90, 0.0) #+# (100, 0.0)) (0, 0.0),
      testDimProbWithName "(90, 0.0) #+# (100, 3.0)" ((90, 0.0) #+# (100, 3.0)) (100, 3.0),
      testDimProbWithName "(0, 0.3) #*# (0, 0.1)" ((0, 0.3) #*# (0, 0.1)) (0, 0.03),
      testDimProbWithName "(0, 0.3) #*# (1, 0.1)" ((0, 0.3) #*# (1, 0.1)) (1, 0.03),
      testDimProbWithName "(1, 0.211) #*# (1, 0.1)" ((1, 0.211) #*# (1, 0.1)) (2, 0.0211),
      testDimProbWithName "(0, 1.0) #-# (0, 0.3)" ((0, 1.0) #-# (0, 0.3)) (0, 0.7),
      testDimProbWithName "(1, 1.0) #-# (2, 0.3)" ((1, 1.0) #-# (2, 0.3)) (1, 1.0),
      testDimProbWithName "(2, 1.0) #-# (2, 0.3)" ((2, 1.0) #-# (2, 0.3)) (2, 0.7),
      testDimProbWithName "(2, 1.0) #-# (1, 0.0)" ((2, 1.0) #-# (1, 0.0)) (2, 1.0),
      testDimProbWithName "(1, 0.0) #-# (2, 0.4)" ((1, 0.0) #-# (2, 0.4)) (2, 0.0),
      testDimProbWithName "(1, 0.7) #-# (1, 0.4)" ((1, 0.4) #-# (1, 0.7)) (1, 0.0)
    ]