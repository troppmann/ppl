module Assert
  ( assertEither,
    assertLeft,
    assertRight,
    assertEqDimProb,
    defaultErrorMargin,
  )
where

import Test.Tasty
import Test.Tasty.HUnit
import Representation (DimensionalProbability)

assertEither :: (Show a, Show b) => Either a b -> Either a b -> Assertion
assertEither (Left _) (Left _) = return ()
assertEither (Right _) (Right _) = return ()
assertEither either1 either2 = assertFailure msg
  where
    msg = "expected: " <> show either1 <> "\n but got: " <> show either2

assertRight :: (Show a) => Either a b -> IO b
assertRight (Left error) = assertFailure $ show error
assertRight (Right value) = return value

assertLeft :: (Show b) => Either a b -> IO a
assertLeft (Left error) = return error
assertLeft (Right value) = assertFailure $ show value

assertEqDimProb :: DimensionalProbability -> DimensionalProbability -> Assertion
assertEqDimProb value@(dimValue,probValue) expected@(dimExpected,probExpected)
  | dimValue /= dimValue = assertFailure failureMsg
  | abs (probExpected-probValue) > defaultErrorMargin = assertFailure failureMsg
  | otherwise = return ()
  where
      failureMsg = "expected: " ++ show expected ++ "\n but got: " ++ show value



defaultErrorMargin = 0.001