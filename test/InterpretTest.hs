module InterpretTest
  ( tests,
  )
where

import Assert
import Interpret
import Parser
import Representation
import Test.HUnit.Approx
import Test.Tasty
import Test.Tasty.HUnit

testInterpretExprEq :: TestName -> Value -> DimensionalProbability -> TestTree
testInterpretExprEq exprString inputValue (expectedDim, expectedProb) = testCase testString $ do
  expr <- assertRight $ parseExpr exprString
  (dim, prob) <- assertRight $ interpret expr inputValue
  dim @?= expectedDim
  assertApproxEqual "" defaultErrorMargin expectedProb prob
  where
    testString = exprString <> ":Value " <> showShorter inputValue

testInterpretExprFail :: TestName -> Value -> String -> TestTree
testInterpretExprFail exprString inputValue expectedError = testCase testString $ do
  expr <- assertRight $ parseExpr exprString
  interpret expr inputValue @?= Left expectedError
  where
    testString = exprString <> ":" <> show inputValue <> ":Expected Left"

showShorter :: Value -> String
showShorter (VFloat float) = show float
showShorter (VBool bool) = show bool
showShorter (VTuple v1 v2) = "(" <> showShorter v1 <> "," <> showShorter v2 <> ")"

tests =
  testGroup
    "Interpret"
    [ testInterpretExprEq "Uniform" (VFloat 0.5) (1, 1.0),
      testInterpretExprEq "Uniform" (VFloat (-10.0)) (1, 0.0),
      testInterpretExprEq "Uniform" (VFloat 1.0) (1, 1.0),
      testInterpretExprEq "Uniform" (VBool True) (0, 0.0),
      testInterpretExprEq "Uniform" (VBool False) (0, 0.0),
      testInterpretExprEq "Normal" (VFloat 0.0) (1, 0.3989),
      testInterpretExprEq "Uniform * 5" (VFloat 0.0) (1, 0.2),
      testInterpretExprFail "Normal / Normal" (VFloat 0.0) "Can only interpret Divide(/) with a one side Constant.",
      testInterpretExprFail "(Normal * 10) / (Normal + 10)" (VFloat 0.0) "Can only interpret Divide(/) with a one side Constant.",
      testInterpretExprEq "Normal + 10" (VFloat 10.0) (1, 0.3989),
      testInterpretExprEq "(3 + Normal * 2)* 0.0" (VFloat 0.0) (0, 1.0),
      testInterpretExprEq "(Normal * 0)* 3.0" (VFloat 0.0) (0, 1.0),
      testInterpretExprEq "Normal * 3 + 2" (VFloat 2.0) (1, 0.1329),
      testInterpretExprEq "2 + (Normal * 0.0) - 0.0" (VFloat 2.0) (0, 1.0),
      -- TODO 12.09.24: > dim
      -- TODO 12.09.24: if dimension
      testInterpretExprEq "if Uniform > 0.5 then 3 else (Uniform < 0.5)" (VBool True) (1, 0.25),
      testInterpretExprEq "1 / Uniform == 4" (VBool True) (0, 0.00001),
      testInterpretExprEq "1 / Uniform == -4" (VBool False) (0, 1.0),
      testInterpretExprEq "(Uniform * -1) / 0.0 < -20.0" (VBool True) (0, 1.0),
      testInterpretExprEq "(Uniform * -1) != -0.5" (VBool True) (0, 1.0),
      testInterpretExprEq "(Uniform < 0.5 == False" (VBool True) (0, 0.5),
      testInterpretExprEq "4 == Normal" (VBool True) (0, 0.00001),
      testInterpretExprEq "-1 / Normal < -2" (VBool True) (0, 0.80853),
      testInterpretExprEq "3 * Uniform >  0.2" (VBool True) (0, 0.93333),
      testInterpretExprEq "Uniform > 0.5" (VBool True) (0, 0.5),
      testInterpretExprEq "if Uniform > 0.5 then Uniform else 2.0" (VFloat 2.0) (1, 0.5),
      testInterpretExprEq "if Uniform < 0.5 then Normal else 2.0" (VFloat 2.0) (1, 0.5270),
      testInterpretExprEq "(if Uniform > 0.5 then 3 else Normal) > 0" (VBool True) (0, 0.75),
      testInterpretExprEq "Normal * -0.000 <= 0.0" (VBool True) (0, 1.0),
      testInterpretExprEq "(Uniform <= 0.2) || (Uniform <= 0.2)" (VBool True) (0, 0.36),
      testInterpretExprEq "!(Uniform <= (1 / 6))" (VBool True) (0, 5 / 6),
      testInterpretExprEq "(Uniform, Uniform)" (VTuple (VFloat 1.0) (VFloat 0.1)) (2, 1.0),
      testInterpretExprEq "(True, Uniform > 0.5) == (True, False)" (VBool True) (0, 0.5),
      testInterpretExprEq "(Uniform * 4, Uniform * 8)" (VTuple (VFloat 1.0) (VFloat 0.1)) (2, 0.25 * 0.125),
      testInterpretExprEq "(Uniform * 4, Uniform * 8)" (VTuple (VFloat 1.0) (VFloat 20.0)) (2, 0.0),
      testInterpretExprEq "(Uniform * 4, Normal)" (VTuple (VFloat 1.0) (VFloat 0.0)) (2, 0.25 * 0.3989),
      testInterpretExprEq "(Uniform * 4, Normal)" (VTuple (VFloat 1.0) (VFloat 20.0)) (2, 0.000),
      testInterpretExprEq "(Uniform, Normal, Uniform < 0.5)" (VTuple (VFloat 1.0) (VTuple (VFloat 0.0) (VBool True))) (2, 0.3989 * 0.5),
      testInterpretExprEq "((Uniform, Normal), Uniform < 0.5)" (VTuple (VTuple (VFloat 1.0) (VFloat 0.0)) (VBool True)) (2, 0.3989 * 0.5),
      testInterpretExprEq "(Uniform, Normal) == (0.5, 0.4)" (VBool True) (2, 1.0 * 0.3684),
      testInterpretExprEq "if Uniform < 0.5 then (Uniform, 2.0) else (Normal, 3.0)" (VTuple (VFloat 1.0) (VFloat 2.0)) (1, 0.5)
    ]