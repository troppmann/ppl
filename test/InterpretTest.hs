module InterpretTest
  ( tests,
  )
where

import Interpret
import Parser
import Representation
import Test.HUnit.Approx
import Test.Tasty
import Test.Tasty.HUnit

errorMargin = 0.001

assertEither :: (Show a, Show b) => Either a b -> Either a b -> Assertion
assertEither (Left _) (Left _) = return ()
assertEither (Right _) (Right _) = return ()
assertEither either1 either2 = assertFailure msg
  where
    msg = "expected: " <> show either1 <> "\n but got: " <> show either2

assertExprInterpret :: TestName -> Value -> DimensionalProbability -> TestTree
assertExprInterpret exprString inputValue (expectedDim, expectedProb) = testCase testString $ do
  let expr = parseExpr exprString
  let dimProb = interpret expr inputValue
  assertEither dimProb (Right (expectedDim, expectedProb))
  let Right (dim, prob) = dimProb
  assertApproxEqual "" errorMargin prob expectedProb
  dim @?= expectedDim
  where
    testString = exprString <> ":" <> show inputValue

assertExprInterpretFail :: TestName -> Value -> String -> TestTree
assertExprInterpretFail exprString inputValue expectedError = testCase testString $ do
  let expr = parseExpr exprString
  interpret expr inputValue @?= Left expectedError
  where
    testString = exprString <> ":" <> show inputValue <> ":Expected Left"

tests =
  testGroup
    "Interpret"
    [ assertExprInterpret "Uniform" (VFloat 0.5) (1, 1.0),
      assertExprInterpret "Uniform" (VFloat (-10.0)) (1, 0.0),
      assertExprInterpret "Uniform" (VFloat 1.0) (1, 1.0),
      assertExprInterpret "Uniform" (VBool True) (0, 0.0),
      assertExprInterpret "Uniform" (VBool False) (0, 0.0),
      assertExprInterpret "Normal" (VFloat 0.0) (1, 0.3989),
      assertExprInterpret "Uniform * 5" (VFloat 0.0) (1, 0.2),
      assertExprInterpretFail "Normal / Normal" (VFloat 0.0) "Can only interpret Divide(/) with a one side Constant.",
      assertExprInterpretFail "(Normal * 10) / (Normal + 10)" (VFloat 0.0) "Can only interpret Divide(/) with a one side Constant.",
      assertExprInterpret "Normal + 10" (VFloat 10.0) (1, 0.3989),
      assertExprInterpret "(3 + Normal * 2)* 0.0" (VFloat 0.0) (0, 1.0),
      assertExprInterpret "(Normal * 0)* 3.0" (VFloat 0.0) (0, 1.0),
      assertExprInterpret "Normal * 3 + 2" (VFloat 2.0) (1, 0.1329),
      assertExprInterpret "2 + (Normal * 0.0) - 0.0" (VFloat 2.0) (0, 1.0),
      -- TODO 12.09.24: > dim
      -- TODO 12.09.24: if dimension
      assertExprInterpret "if Uniform > 0.5 then 3 else (Uniform < 0.5)" (VBool True) (1, 0.25),
      assertExprInterpret "1 / Uniform == 4" (VBool True) (0, 0.00001),
      assertExprInterpret "1 / Uniform == -4" (VBool False) (0, 1.0),
      assertExprInterpret "(Uniform * -1) / 0.0 < -20.0" (VBool True) (0, 1.0),
      assertExprInterpret "(Uniform * -1) != -0.5" (VBool True) (0, 1.0),
      assertExprInterpret "(Uniform < 0.5 == False" (VBool True) (0, 0.5),
      assertExprInterpret "4 == Normal" (VBool True) (0, 0.00001),
      assertExprInterpret "-1 / Normal < -2" (VBool True) (0, 0.80853),
      assertExprInterpret "3 * Uniform >  0.2" (VBool True) (0, 0.93333),
      assertExprInterpret "Uniform > 0.5" (VBool True) (0, 0.5),
      assertExprInterpret "if Uniform > 0.5 then Uniform else 2.0" (VFloat 2.0) (1, 0.5),
      assertExprInterpret "if Uniform < 0.5 then Normal else 2.0" (VFloat 2.0) (1, 0.5270),
      assertExprInterpret "(if Uniform > 0.5 then 3 else Normal) > 0" (VBool True) (0, 0.75),
      assertExprInterpret "Normal * -0.000 <= 0.0" (VBool True) (0, 1.0),
      assertExprInterpret "(Uniform <= 0.2) || (Uniform <= 0.2)" (VBool True) (0, 0.36),
      assertExprInterpret "!(Uniform <= (1 / 6))" (VBool True) (0, 5 / 6)
    ]