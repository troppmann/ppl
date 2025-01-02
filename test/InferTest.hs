module InferTest
  ( tests,
  )
where

import Assert
import Infer
import Parser
import Problems
import Representation
import Shorter
import Test.HUnit.Approx
import Test.Tasty
import Test.Tasty.HUnit

testInferExprEq :: String -> Value -> DimensionalProbability -> TestTree
testInferExprEq exprString = testInferExprEqWithName exprString exprString

testInferExprEqWithName :: String -> TestName -> Value -> DimensionalProbability -> TestTree
testInferExprEqWithName exprString testName inputValue expected = testCase testString $ do
  expr <- assertRight $ parseExpr exprString
  let program = wrapInMain expr
  dimProb <- assertRight $ inferProgram program inputValue
  assertEqDimProb dimProb expected
  where
    testString = shorter testName <> ":Value " <> shorter inputValue

testInferExprFail :: TestName -> Value -> String -> TestTree
testInferExprFail exprString inputValue expectedError = testCase testString $ do
  expr <- assertRight $ parseExpr exprString
  let program = wrapInMain expr
  inferProgram program inputValue @?= Left expectedError
  where
    testString = exprString <> ":" <> shorter inputValue <> ":Expected Fail"

testInferProgram :: String -> Value -> DimensionalProbability -> TestTree
testInferProgram programString inputValue expected = testCase testString $ do
  let parseOpt = ParseOptions False 0
  program <- assertRight $ parseProgramWithOptions parseOpt programString
  dimProb <- assertRight $ inferProgram program inputValue
  assertEqDimProb dimProb expected
  where
    testString = shorter programString <> ":Value " <> shorter inputValue

tests =
  testGroup
    "Infer"
    [ testGroup
        "Basic"
        [ testInferExprEq "Uniform" (VFloat 0.5) (1, 1.0),
          testInferExprEq "Uniform" (VFloat 0.0) (1, 1.0),
          testInferExprEq "Uniform" (VFloat 1.0) (1, 0.0),
          testInferExprEq "Uniform" (VFloat (-10.0)) (1, 0.0),
          testInferExprEq "Uniform" (VBool True) (0, 0.0),
          testInferExprEq "Uniform" (VBool False) (0, 0.0),
          testInferExprEq "Normal" (VFloat 0.0) (1, 0.3989),
          testInferExprEq "Uniform * 5" (VFloat 4.8) (1, 0.2),
          testInferExprFail "Normal / Normal" (VFloat 0.0) "Can only infer Divide(/) with a one side Constant.",
          testInferExprFail "(Normal * 10) / (Normal + 10)" (VFloat 0.0) "Can only infer Divide(/) with a one side Constant.",
          testInferExprEq "5/10" (VFloat 0.5) (0, 1.0),
          testInferExprEq "Normal + 10" (VFloat 10.0) (1, 0.3989),
          testInferExprEq "(3 + Normal * 2)* 0.0" (VFloat 0.0) (0, 1.0),
          testInferExprEq "(Normal * 0)* 3.0" (VFloat 0.0) (0, 1.0),
          testInferExprEq "(Normal * 0) + 3.0" (VFloat 3.0) (0, 1.0),
          testInferExprEq "Normal * 3 + 2" (VFloat 2.0) (1, 0.1329),
          testInferExprEq "2 + (Normal * 0.0) - 0.0" (VFloat 2.0) (0, 1.0),
          testInferExprEq "if Uniform > 0.5 then 3 else (Uniform < 0.5)" (VBool True) (0, 0.25),
          testInferExprEq "4 * Uniform == 0.7" (VBool True) (1, 0.25),
          testInferExprEq "4 * Uniform == 0.7" (VBool False) (0, 1.0),
          testInferExprEq "4 * Uniform != 0.7" (VBool False) (1, 0.25),
          testInferExprEq "4 * Uniform != 0.7" (VBool True) (0, 1.0),
          testInferExprEq "1 / Uniform" (VFloat 4.0) (1, 1 * (1 / (4.0 ^ 2))),
          testInferExprEq "3 / (2 * Uniform) == 4" (VBool True) (1, 0.5 * (3 / (4.0 ^ 2))),
          testInferExprEq "1 / Uniform == -4" (VBool True) (1, 0.0),
          testInferExprEq "(Uniform * -1) / 0.0 < -20.0" (VBool True) (0, 1.0),
          testInferExprEq "(Uniform * -1) == -0.5" (VBool False) (0, 1.0),
          testInferExprEq "(Uniform * -1) != -0.5" (VBool True) (0, 1.0),
          testInferExprEq "(Uniform < 0.5 == False" (VBool True) (0, 0.5),
          testInferExprEq "1 == Normal" (VBool True) (1, 0.24197),
          testInferExprEq "Normal != 0" (VBool True) (0, 1.0),
          testInferExprEq "Normal != 0" (VBool False) (1, 0.3989),
          testInferExprEq "Normal" (VFloat 1.0) (1, 0.24197),
          testInferExprEq "-1 / Normal < -2" (VBool True) (0, 0.80853),
          testInferExprEq "3 * Uniform >  0.2" (VBool True) (0, 0.93333),
          testInferExprEq "Uniform > 0.5" (VBool True) (0, 0.5),
          testInferExprEq "if Uniform > 0.5 then Uniform else 2.0" (VFloat 2.0) (0, 0.5),
          testInferExprEq "if Uniform < 0.5 then Normal else 2.0" (VFloat 2.0) (0, 0.5),
          testInferExprEq "(if Uniform > 0.5 then 3 else Normal) > 0" (VBool True) (0, 0.75),
          testInferExprEq "Normal * -0.000 <= 0.0" (VBool True) (0, 1.0),
          testInferExprEq "(Uniform <= 0.2) || (Uniform <= 0.2)" (VBool True) (0, 0.36),
          testInferExprEq "!(Uniform <= (1 / 6))" (VBool True) (0, 5 / 6),
          testInferExprEq "(True, Uniform > 0.5) == (True, False)" (VBool True) (0, 0.5),
          testInferExprEq "if Uniform < 0.5 then (Uniform, 2.0) else (Normal, 3.0)" (VTuple (VFloat 0.9) (VFloat 2.0)) (1, 0.5),
          testInferExprEq "False || (False && True)" (VBool True) (0, 0.0),
          testInferExprEq "False && Uniform" (VBool True) (0, 0.0),
          testInferExprEq "Uniform ** 0" (VFloat 1.0) (0, 1.0),
          testInferExprEq "Uniform ** 2" (VFloat 2.0) (1, 0.0),
          testInferExprEq "Uniform ** 2" (VFloat 0.5) (1, 0.7071067811865),
          testInferExprEq "Uniform ** 0.5 <= 0.5" (VBool True) (0, 0.25),
          testInferExprEq "4 ** Uniform <= 4" (VBool True) (0, 1.0),
          testInferExprEq "2 ** Normal < -0.5" (VBool True) (0, 0.0),
          testInferExprEq "2 ** Normal" (VFloat 2) (1,0.1745),
          testInferExprEq "abs (Uniform - 0.5) > 0.25" (VBool True) (0, 0.5),
          testInferExprEq "-(abs (Uniform - 0.5)) > 0.25" (VBool True) (0, 0.0),
          testInferExprEq "True || 3" (VBool True) (0, 1.0),
          testInferExprEq "False || 3" (VBool True) (0, 0.0)
        ],
      testGroup
        "Dimension"
        [ testInferExprEq "(Uniform, Uniform)" (VTuple (VFloat 0.4) (VFloat 0.1)) (2, 1.0),
          testInferExprEq "(Uniform * 4, Uniform * 8)" (VTuple (VFloat 1.0) (VFloat 0.1)) (2, 0.25 * 0.125),
          testInferExprEq "(Uniform * 4, Uniform * 8)" (VTuple (VFloat 1.0) (VFloat 20.0)) (2, 0.0),
          testInferExprEq "(Uniform * 4, Normal)" (VTuple (VFloat 1.0) (VFloat 0.0)) (2, 0.25 * 0.3989),
          testInferExprEq "(Uniform * 4, Normal)" (VTuple (VFloat 1.0) (VFloat 20.0)) (2, 0.000),
          testInferExprEq "(Uniform, Normal, Uniform < 0.5)" (VTuple (VFloat 0.1) (VTuple (VFloat 0.0) (VBool True))) (2, 0.3989 * 0.5),
          testInferExprEq "((Uniform, Normal), Uniform < 0.5)" (VTuple (VTuple (VFloat 0.1) (VFloat 0.0)) (VBool True)) (2, 0.3989 * 0.5),
          testInferExprEq "(Uniform, Normal + 1, Uniform * 2)" (VTuple (VFloat 0.5) (VTuple (VFloat 1.0) (VFloat 1.0))) (3, 1.0 * 0.3989 * 0.5),
          testInferExprEq "(Uniform, Normal) == (0.5, 0.4)" (VBool True) (2, 1.0 * 0.3684),
          testInferExprEq "if Uniform < 0.5 then (Uniform > 0.5, Uniform * 2) else (Uniform * 3 > 0.5, Uniform * 2 + 1)" (VTuple (VBool True) (VFloat 1.5)) (1, 1 / 3),
          testInferExprEq "if 2 * Uniform == 0.5 then 1 + (Uniform * 5) else 4 * Uniform" (VFloat 0.2) (1, 0.25),
          testInferExprEq "if 2 * Uniform == 0.5 then 1 + (Uniform * 5) else 4 * Uniform" (VFloat 5.7) (2, 0.5 * 0.2),
          testInferExprEq "if 2 * Uniform == 0.5 then 1 + (Uniform * 5) else 4 * Uniform" (VFloat 3.0) (1, 0.25),
          testInferExprEq "2 * Uniform == 0.3" (VBool True) (1, 0.5),
          testInferExprEq "2 * Uniform == 0.3 || False" (VBool True) (1, 0.5),
          testInferExprEq "2 * Uniform == 0.3 && True" (VBool True) (1, 0.5),
          testInferExprEq "2 * Uniform == 0.3 && (4 * Uniform == 0.8)" (VBool True) (2, 0.5 * 0.25),
          testInferExprEq "if Uniform != 0.3 then True else False" (VBool True) (0, 1.0),
          testInferExprEq "if Uniform != 0.3 then True else False" (VBool False) (1, 1.0),
          testInferExprEq "Uniform" (VTuple (VFloat 0.5) (VFloat 0.5)) (0, 0.0),
          testInferExprEq "Uniform + 1" (VTuple (VFloat 0.5) (VFloat 0.5)) (0, 0.0)
        ],
      testGroup
        "Problem"
        [ testInferExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 0.0) (VFloat 0.5)) (1, 0.5 * 0.99 * 0.25),
          testInferExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 0.0) (VFloat 4.0)) (0, 0.5 * 0.01),
          testInferExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 0.0) (VFloat 9.5)) (0, 0.0),
          testInferExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 0.0) (VFloat 10.0)) (0, 0.0),
          testInferExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 0.0) (VFloat 12.0)) (0, 0.0),
          testInferExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 1.0) (VFloat 3.5)) (1, 0.5 * 0.99 * 0.1),
          testInferExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 1.0) (VFloat 4.0)) (1, 0.5 * 0.99 * 0.1),
          testInferExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 1.0) (VFloat 9.5)) (1, 0.5 * 0.99 * 0.1),
          testInferExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 1.0) (VFloat 10.0)) (0, 0.5 * 0.01),
          testInferExprEqWithName indiaGpaProblem "IndiaGpaProblem(..)" (VTuple (VFloat 1.0) (VFloat 12.0)) (0, 0.0),
          testInferExprEqWithName add2Uniform "Add2Uniform(..)" (VFloat 1.0) (1, 1.0),
          testInferExprEqWithName add2Uniform "Add2Uniform(..)" (VFloat 0.5) (1, 0.5)
        ],
      testGroup
        "Programs"
        [ testInferProgram "main = 2 + 2" (VFloat 4.0) (0, 1.0),
          testInferProgram "main = mult2 3; mult2 x = x + x;" (VFloat 6.0) (0, 1.0),
          testInferProgram "main = test Uniform; test x = if x < 0.5 then 1 else 2;" (VFloat 1.0) (0, 0.5),
          testInferProgram "main = test Uniform; test x = if x < 0.5 then 1 else 2;" (VFloat 4.0) (0, 0.0),
          testInferProgram "main = replicate 1 Uniform; replicate n value = if n <= 1 then value else (value, replicate (n-1) value);" (VFloat 3.0) (0, 0.0),
          testInferProgram "main = replicate 1 Uniform; replicate n value = if n <= 1 then value else (value, replicate (n-1) value);" (VFloat 0.5) (1, 1.0),
          testInferProgram "main = (Uniform, Uniform)" (VTuple (VFloat 0.5) (VFloat 0.5)) (2, 1.0),
          testInferProgram "main = dice 40;dice n = if n <= 1 then 1 else ( if Uniform < (1/n) then n else dice (n-1));" (VFloat 20) (1, 1.0/40.0),
          testInferProgram "main = replicate 2 (Uniform + 1);replicate n value = if n <= 1 then value else (value, replicate (n-1) value);" (VTuple (VFloat 1.5) (VFloat 1.5)) (2, 1.0),
          testInferProgram "main = pair (Uniform); pair x = (x,x)" (VTuple (VFloat 0.5) (VFloat 0.5)) (2, 1.0),
          testInferProgram "main = tMid (tIn);tIn = 1; tMid x = (x,x)" (VTuple (VFloat 1.0) (VFloat 1.0)) (1, 1.0),
          testInferProgram "main = replicate 2 (dice 6);dice n = if n <= 1 then 1 else ( if Uniform < (1/n) then n else dice (n-1));replicate n value = if n <= 1 then value else (value, replicate (n-1) value);" (VTuple (VFloat 3) (VFloat 2)) (0, 1.0/(6.0 * 6.0)),
          testInferProgram "main = replicate (dice 3) 2;dice n = if n <= 1 then 1 else ( if Uniform < (1/n) then n else dice (n-1));replicate n value = if n <= 1 then value else (value, replicate (n-1) value);" (VTuple (VFloat 2) (VFloat 2)) (0, (2/3) * (2/3)),
          testInferProgram "main = (Uniform, Uniform, Uniform)" (VTuple (VFloat 0.5) (VTuple (VFloat 0.5) (VFloat 0.5))) (3, 1.0)
        ]
    ]