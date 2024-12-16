module InterpretTest
  ( tests,
  )
where

import Assert
import Interpret
import Parser
import Problems
import Representation
import Shorter
import Test.HUnit.Approx
import Test.Tasty
import Test.Tasty.HUnit

testInterpretExprEq :: String -> Value -> DimensionalProbability -> TestTree
testInterpretExprEq exprString = testInterpretExprEqWithName exprString exprString

testInterpretExprEqWithName :: String -> TestName -> Value -> DimensionalProbability -> TestTree
testInterpretExprEqWithName exprString testName inputValue (expectedDim, expectedProb) = testCase testString $ do
  expr <- assertRight $ parseExpr exprString
  (dim, prob) <- assertRight $ interpret expr inputValue
  dim @?= expectedDim
  assertApproxEqual "" defaultErrorMargin expectedProb prob
  where
    testString = shorter testName <> ":Value " <> shorter inputValue

testInterpretExprFail :: TestName -> Value -> String -> TestTree
testInterpretExprFail exprString inputValue expectedError = testCase testString $ do
  expr <- assertRight $ parseExpr exprString
  interpret expr inputValue @?= Left expectedError
  where
    testString = exprString <> ":" <> shorter inputValue <> ":Expected Fail"

tests =
  testGroup
    "Interpret"
    [ testGroup
        "Basic"
        [ testInterpretExprEq "Uniform" (VFloat 0.5) (1, 1.0),
          testInterpretExprEq "Uniform" (VFloat 0.0) (1, 1.0),
          testInterpretExprEq "Uniform" (VFloat 1.0) (1, 1.0),
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
          testInterpretExprEq "(Normal * 0) + 3.0" (VFloat 3.0) (0, 1.0),
          testInterpretExprEq "Normal * 3 + 2" (VFloat 2.0) (1, 0.1329),
          testInterpretExprEq "2 + (Normal * 0.0) - 0.0" (VFloat 2.0) (0, 1.0),
          testInterpretExprEq "if Uniform > 0.5 then 3 else (Uniform < 0.5)" (VBool True) (0, 0.25),
          testInterpretExprEq "4 * Uniform == 0.7" (VBool True) (1, 0.25),
          testInterpretExprEq "4 * Uniform == 0.7" (VBool False) (0, 1.0),
          testInterpretExprEq "4 * Uniform != 0.7" (VBool False) (1, 0.25),
          testInterpretExprEq "4 * Uniform != 0.7" (VBool True) (0, 1.0),
          testInterpretExprEq "1 / Uniform" (VFloat 4.0) (1, 1 * (1 / (4.0 ^ 2))),
          testInterpretExprEq "3 / (2 * Uniform) == 4" (VBool True) (1, 0.5 * (3 / (4.0 ^ 2))),
          testInterpretExprEq "1 / Uniform == -4" (VBool True) (1, 0.0),
          testInterpretExprEq "(Uniform * -1) / 0.0 < -20.0" (VBool True) (0, 1.0),
          testInterpretExprEq "(Uniform * -1) == -0.5" (VBool False) (0, 1.0),
          testInterpretExprEq "(Uniform * -1) != -0.5" (VBool True) (0, 1.0),
          testInterpretExprEq "(Uniform < 0.5 == False" (VBool True) (0, 0.5),
          testInterpretExprEq "1 == Normal" (VBool True) (1, 0.24197),
          testInterpretExprEq "Normal != 0" (VBool True) (0, 1.0),
          testInterpretExprEq "Normal != 0" (VBool False) (1, 0.3989),
          testInterpretExprEq "Normal" (VFloat 1.0) (1, 0.24197),
          testInterpretExprEq "-1 / Normal < -2" (VBool True) (0, 0.80853),
          testInterpretExprEq "3 * Uniform >  0.2" (VBool True) (0, 0.93333),
          testInterpretExprEq "Uniform > 0.5" (VBool True) (0, 0.5),
          testInterpretExprEq "if Uniform > 0.5 then Uniform else 2.0" (VFloat 2.0) (0, 0.5),
          testInterpretExprEq "if Uniform < 0.5 then Normal else 2.0" (VFloat 2.0) (0, 0.5),
          testInterpretExprEq "(if Uniform > 0.5 then 3 else Normal) > 0" (VBool True) (0, 0.75),
          testInterpretExprEq "Normal * -0.000 <= 0.0" (VBool True) (0, 1.0),
          testInterpretExprEq "(Uniform <= 0.2) || (Uniform <= 0.2)" (VBool True) (0, 0.36),
          testInterpretExprEq "!(Uniform <= (1 / 6))" (VBool True) (0, 5 / 6),
          testInterpretExprEq "(True, Uniform > 0.5) == (True, False)" (VBool True) (0, 0.5),
          testInterpretExprEq "if Uniform < 0.5 then (Uniform, 2.0) else (Normal, 3.0)" (VTuple (VFloat 1.0) (VFloat 2.0)) (1, 0.5),
          testInterpretExprEq "False || (False && True)" (VBool True) (0, 0.0),
          testInterpretExprEq "False && Uniform" (VBool True) (0, 0.0),
          testInterpretExprEq "Uniform ** 0" (VFloat 1.0) (0, 1.0),
          testInterpretExprEq "Uniform ** 2" (VFloat 2.0) (1, 0.0),
          testInterpretExprEq "Uniform ** 2" (VFloat 0.5) (1, 0.7071067811865),
          testInterpretExprEq "Uniform ** 0.5 <= 0.5" (VBool True) (0, 0.25),
          testInterpretExprEq "True || 3" (VBool True) (0, 1.0),
          testInterpretExprEq "False || 3" (VBool True) (0, 0.0)
        ],
      testGroup
        "Dimension:"
        [ testInterpretExprEq "(Uniform, Uniform)" (VTuple (VFloat 1.0) (VFloat 0.1)) (2, 1.0),
          testInterpretExprEq "(Uniform * 4, Uniform * 8)" (VTuple (VFloat 1.0) (VFloat 0.1)) (2, 0.25 * 0.125),
          testInterpretExprEq "(Uniform * 4, Uniform * 8)" (VTuple (VFloat 1.0) (VFloat 20.0)) (2, 0.0),
          testInterpretExprEq "(Uniform * 4, Normal)" (VTuple (VFloat 1.0) (VFloat 0.0)) (2, 0.25 * 0.3989),
          testInterpretExprEq "(Uniform * 4, Normal)" (VTuple (VFloat 1.0) (VFloat 20.0)) (2, 0.000),
          testInterpretExprEq "(Uniform, Normal, Uniform < 0.5)" (VTuple (VFloat 1.0) (VTuple (VFloat 0.0) (VBool True))) (2, 0.3989 * 0.5),
          testInterpretExprEq "((Uniform, Normal), Uniform < 0.5)" (VTuple (VTuple (VFloat 1.0) (VFloat 0.0)) (VBool True)) (2, 0.3989 * 0.5),
          testInterpretExprEq "(Uniform, Normal + 1, Uniform * 2)" (VTuple (VFloat 0.5) (VTuple (VFloat 1.0) (VFloat 1.0))) (3, 1.0 * 0.3989 * 0.5),
          testInterpretExprEq "(Uniform, Normal) == (0.5, 0.4)" (VBool True) (2, 1.0 * 0.3684),
          testInterpretExprEq "if Uniform < 0.5 then (Uniform > 0.5, Uniform * 2) else (Uniform * 3 > 0.5, Uniform * 2 + 1)" (VTuple (VBool True) (VFloat 1.5)) (1, 1 / 3),
          testInterpretExprEq "if 2 * Uniform == 0.5 then 1 + (Uniform * 5) else 4 * Uniform" (VFloat 0.2) (1, 0.25),
          testInterpretExprEq "if 2 * Uniform == 0.5 then 1 + (Uniform * 5) else 4 * Uniform" (VFloat 5.7) (2, 0.5 * 0.2),
          testInterpretExprEq "if 2 * Uniform == 0.5 then 1 + (Uniform * 5) else 4 * Uniform" (VFloat 3.0) (1, 0.25),
          testInterpretExprEq "2 * Uniform == 0.3" (VBool True) (1, 0.5),
          testInterpretExprEq "2 * Uniform == 0.3 || False" (VBool True) (1, 0.5),
          testInterpretExprEq "2 * Uniform == 0.3 && True" (VBool True) (1, 0.5),
          testInterpretExprEq "2 * Uniform == 0.3 && (4 * Uniform == 0.8)" (VBool True) (2, 0.5 * 0.25),
          testInterpretExprEq "if Uniform != 0.3 then True else False" (VBool True) (0, 1.0),
          testInterpretExprEq "if Uniform != 0.3 then True else False" (VBool False) (1, 1.0)
        ],
      testGroup
        "Problem"
        [ testInterpretExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 0.0) (VFloat 0.5)) (1, 0.5 * 0.99 * 0.25),
          testInterpretExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 0.0) (VFloat 4.0)) (0, 0.5 * 0.01),
          testInterpretExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 0.0) (VFloat 9.5)) (0, 0.0),
          testInterpretExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 0.0) (VFloat 10.0)) (0, 0.0),
          testInterpretExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 0.0) (VFloat 12.0)) (0, 0.0),
          testInterpretExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 1.0) (VFloat 3.5)) (1, 0.5 * 0.99 * 0.1),
          testInterpretExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 1.0) (VFloat 4.0)) (1, 0.5 * 0.99 * 0.1),
          testInterpretExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 1.0) (VFloat 9.5)) (1, 0.5 * 0.99 * 0.1),
          testInterpretExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 1.0) (VFloat 10.0)) (0, 0.5 * 0.01),
          testInterpretExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 1.0) (VFloat 12.0)) (0, 0.0)
        ]
    ]