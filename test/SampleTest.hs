module SampleTest
  ( tests,
  )
where

import Parser
import Representation
import Sample
import Test.Tasty
import Test.Tasty.HUnit

assertExprEq :: TestName -> Value -> TestTree
assertExprEq exprString expectedValue = testCase exprString $ do
  let expr = parseExpr exprString
  sample <- sampleExpr expr
  sample @?= expectedValue

-- TODO 11.09.24: swap with QuickCheck test
assertExprInRange :: TestName -> (Double, Double) -> TestTree
assertExprInRange exprString (low, high) = testCase testString $ do
  let expr = parseExpr exprString
  VFloat sample <- sampleExpr expr
  assertBool "" $ sample >= low
  assertBool "" $ sample <= high
  where
    testString = exprString <> ":InRange[" <> show low <> "," <> show high <> "]"

infinity = 1 / 0

tests =
  testGroup
    "Sample"
    [ assertExprEq "True" $ VBool True,
      assertExprEq "False" $ VBool False,
      assertExprEq "3" $ VFloat 3.0,
      assertExprEq "-3" $ VFloat (-3.0),
      assertExprEq "3 * 4 + 12" $ VFloat 24.0,
      assertExprEq "3 + 4 * 12" $ VFloat 84.0,
      assertExprEq "(3 + 4) * 12" $ VFloat 84.0,
      assertExprEq "3 * (4 + 12)" $ VFloat 48.0,
      assertExprEq "if True then 3.0 else 2.0" $ VFloat 3.0,
      assertExprEq "if False then 3.0 else 2.0" $ VFloat 2.0,
      assertExprEq "True && ! False && False" $ VBool False,
      assertExprEq "if True && !(False!=False) || False then 3.0 else 2.0" $ VFloat 3.0,
      assertExprInRange "Uniform" (0.0, 1.0),
      assertExprInRange "Uniform * 4" (0.0, 4.0),
      assertExprInRange "Uniform * 4 + 10" (10.0, 14.0),
      assertExprInRange "Uniform * (4 + 10)" (0.0, 14.0),
      assertExprInRange "Normal" (-infinity, infinity),
      assertExprEq "(3 + Normal * 2)* 0.0" (VFloat 0.0),
      assertExprEq "(Normal * 0)* 3.0" (VFloat 0.0),
      assertExprEq "2 + (Normal * 0.0) - 0.0" (VFloat 2.0)
    ]
