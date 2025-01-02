module SampleTest
  ( tests,
  )
where

import Assert
import Parser
import Problems
import Representation
import Sample
import Test.Tasty
import Test.Tasty.HUnit

testSampleExprEq :: TestName -> Value -> TestTree
testSampleExprEq exprString expectedValue = testCase exprString $ do
  expr <- assertRight $ parseExpr exprString
  let program = wrapInMain expr
  sample <- sampleProgram program
  sample @?= expectedValue

-- TODO 11.09.24: swap with QuickCheck test
testSampleExprInRange :: TestName -> (Double, Double) -> TestTree
testSampleExprInRange exprString (low, high) = testCase testString $ do
  expr <- assertRight $ parseExpr exprString
  let program = wrapInMain expr
  VFloat sample <- sampleProgram program
  assertBool "" $ sample >= low
  assertBool "" $ sample <= high
  where
    testString = exprString <> ":InRange[" <> show low <> "," <> show high <> "]"

testSampleExpr :: String -> TestTree
testSampleExpr exprString = testSampleExprWithName exprString exprString

testSampleExprWithName :: String -> TestName -> TestTree
testSampleExprWithName exprString testName = testCase testName $ do
  expr <- assertRight $ parseExpr exprString
  let program = wrapInMain expr
  _sample <- sampleProgram program
  return ()

tests =
  testGroup
    "Sample"
    [ testSampleExprEq "True" $ VBool True,
      testSampleExprEq "False" $ VBool False,
      testSampleExprEq "3" $ VFloat 3.0,
      testSampleExprEq "-3" $ VFloat (-3.0),
      testSampleExprEq "3 * 4 + 12" $ VFloat 24.0,
      testSampleExprEq "3 + 4 * 12" $ VFloat 84.0,
      testSampleExprEq "3 ** 4" $ VFloat 81.0,
      testSampleExprEq "(3 + 4) * 12" $ VFloat 84.0,
      testSampleExprEq "3 * (4 + 12)" $ VFloat 48.0,
      testSampleExprEq "if True then 3.0 else 2.0" $ VFloat 3.0,
      testSampleExprEq "if False then 3.0 else 2.0" $ VFloat 2.0,
      testSampleExprEq "True && ! False && False" $ VBool False,
      testSampleExprEq "if True && !(False!=False) || False then 3.0 else 2.0" $ VFloat 3.0,
      testSampleExprInRange "Uniform" (0.0, 1.0),
      testSampleExprInRange "Uniform * 4" (0.0, 4.0),
      testSampleExprInRange "Uniform ** 4" (0.0, 1.0),
      testSampleExprInRange "4 ** Uniform * 2" (0.0, 16.0),
      testSampleExprInRange "Uniform * 4 + 10" (10.0, 14.0),
      testSampleExprInRange "Uniform * (4 + 10)" (0.0, 14.0),
      testSampleExprInRange "abs (Uniform - 0.5)" (-0.5, 0.5),
      testSampleExpr "Normal",
      testSampleExprEq "(3 + Normal * 2)* 0.0" (VFloat 0.0),
      testSampleExprEq "(Normal * 0)* 3.0" (VFloat 0.0),
      testSampleExprEq "2 + (Normal * 0.0) - 0.0" (VFloat 2.0),
      testSampleExprEq "False && Uniform" (VBool False),
      testSampleExprEq "True || 3" (VBool True),
      testSampleExprWithName indiaGpaProblem "IndiaGpaProblem(..)"
    ]
