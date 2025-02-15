module MapTest
  ( tests,
  )
where

import Assert
import MaximumAPosteriori
import Parser
import Representation
import Shorter
import Test.HUnit.Approx
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

type QueryString = String

type ExprString = String

type ErrorString = String

testMapExpr :: TestName -> QueryString -> Value -> TestTree
testMapExpr testName = testMapExprWithName testName testName

testMapProgram :: ExprString -> QueryString -> Value -> DimensionalProbability -> TestTree
testMapProgram programString queryString expectedValue expectedDimProb = testCase testString $ do
  program <- assertRight $ parseProgram programString
  query <- assertRight $ parseQuery queryString
  (dimProb, value) <- assertRight $ mmap program query
  value @?= expectedValue
  assertEqDimProb dimProb expectedDimProb
  where
    testString = shorter programString <> ":Query " <> shorter queryString

testMapExprWithName :: ExprString -> TestName -> QueryString -> Value -> TestTree
testMapExprWithName programString testName queryString expectedValue = testCase testString $ do
  expr <- assertRight $ parseExpr programString
  query <- assertRight $ parseQuery queryString
  let program = wrapInMain expr
  (_dimProb, value) <- assertRight $ mmap program query
  value @?= expectedValue
  where
    testString = shorter testName <> ":Query " <> shorter queryString

testMapExprFail :: ExprString -> QueryString -> DimensionalProbability -> TestTree
testMapExprFail exprString queryString expected = testCase testString $ do
  expr <- assertRight $ parseExpr exprString
  query <- assertRight $ parseQuery queryString
  let program = wrapInMain expr
  (dimProb, _value) <- assertRight $ mmap program query
  assertEqDimProb dimProb expected
  where
    testString = shorter exprString <> ":Query " <> shorter queryString <> ":Expected 0.0 Prob"

tests =
  testGroup
    "MaximumAPosteriori"
    [ testGroup
        "MAP without evidence p(q)"
        [ testMapExpr "3" "_" (VFloat 3.0),
          testMapExpr "Normal" "_" (VFloat 0.0),
          testMapExpr "Uniform" "_" (VFloat 0.5),
          testMapExpr "Normal * 3 + 7" "_" (VFloat 7.0),
          testMapExpr "(Normal + 7) * 3" "_" (VFloat 21.0),
          testMapExpr "((Normal + 7) * 3 - 1) / 2" "_" (VFloat 10.0),
          testMapExpr "if Uniform < 0.2 then 3 * 9 else 7" "_" (VFloat 7),
          testMapExpr "if Uniform < 0.3 then 5 else (if Uniform < 0.49 then 2 else 6)" "_" (VFloat 6.0),
          testMapExpr "if Uniform == 0.5 then 10 else 0" "_" (VFloat 0.0),
          testMapExpr "if Uniform != 3 then 10 else 0" "_" (VFloat 10.0),
          testMapExpr "if Uniform < 0.5 then 3 else Normal" "_" (VFloat 3.0),
          testMapExpr "if Uniform < 0.5 then Normal + 0.01 else Normal - 0.01" "_" (VFloat (-0.01)),
          testMapExpr "(Normal * 10 + 5, Normal * 5)" "_" (VTuple (VFloat 5) (VFloat 0)),
          testMapExpr "(Normal * 10 + 5, if Uniform > 0.7 then Normal + 8 else 0)" "_" (VTuple (VFloat 5) (VFloat 0)),
          testMapExpr "(if Uniform > 0.7 then 0 else Normal, if Uniform == 0.7 then 8 else 0)" "_" (VTuple (VFloat 0) (VFloat 0)),
          testMapExpr "(Normal, if Uniform > 0.7 then Normal else 0, 8)" "_" (VTuple (VFloat 0) (VTuple (VFloat 0) (VFloat 8))),
          testMapExpr "if (Uniform < 0.4) || (Uniform < 0.4) then (5,5) else (Normal + 10, 10)" "_" (VTuple (VFloat 5) (VFloat 5)),
          testMapExpr "if Uniform < 0.6 then Normal * 10 else 1 + (Normal * 0.0) " "_" (VFloat 1.0),
          testMapExpr "if Uniform < 0.3 then Normal else Uniform * 100" "_" (VFloat 0.0),
          testMapExpr "if Normal != 0.0 then (if Uniform == 0.4 then 5 else 8, 5) else (10, 10)" "_" (VTuple (VFloat 8) (VFloat 5))
        ],
      testGroup
        "MAP with evidence p(q,e)"
        [ testMapExpr "(Normal, 3)" "(_, 3)" (VTuple (VFloat 0) (VFloat 3)),
          testMapExprFail "(Normal, 3)" "(_, 4)" (1, 0.0),
          testMapExprFail "(Uniform + 4, 3)" "(3.5, _)" (1, 0.0),
          testMapExprFail "Uniform" "(4)" (1, 0.0),
          testMapExpr "Uniform + 3.5" "(4)" (VFloat 4),
          testMapExpr "(Normal, Normal)" "(_, 3)" (VTuple (VFloat 0) (VFloat 3)),
          testMapExpr "(Normal, Normal)" "(3, _)" (VTuple (VFloat 3) (VFloat 0)),
          testMapExpr "(Normal, Normal + 7)" "(3, _)" (VTuple (VFloat 3) (VFloat 7)),
          testMapExpr "if Uniform < 0.5 then (Normal + 3, 10) else (Normal + 7, 0)" "(_, 10)" (VTuple (VFloat 3) (VFloat 10)),
          testMapExpr "if Uniform < 0.4 then (Normal + 3, 10) else (Normal + 7, 0)" "(3, _)" (VTuple (VFloat 3) (VFloat 10)),
          testMapExpr "if Uniform < 0.5 then (Normal + 3, 10) else (3, 0)" "(3, _)" (VTuple (VFloat 3) (VFloat 0)),
          testMapExpr "if Uniform < 0.2 then (Uniform, Normal) else (Uniform + 3, Normal)" "(3.1, _)" (VTuple (VFloat 3.1) (VFloat 0.0)),
          testMapExpr "if Uniform < 0.8 then (Uniform, Normal) else (Uniform + 3, Normal)" "(_, _)" (VTuple (VFloat 0.5) (VFloat 0.0)),
          testMapExpr "if Uniform < 0.2 then Uniform else Uniform + 3" "(_)" (VFloat 3.5),
          testMapExpr "if Uniform == 0.2 then 1 else Normal" "(_)" (VFloat 1.0),
          testMapProgram "main = if Uniform < (5/7) then (0,poisson 10) else (1,poisson 3);exp v = 2.718281828 ** v; poisson tau = innerPoisson tau Uniform 0 (exp (-tau)) (exp (-tau)); innerPoisson tau u x p s = if u > s then innerPoisson tau (Uniform * (1 - s) +s) (x+1) (p*tau/ (x+1)) (s + (p*tau/ (x+1))) else x" "(_, 4)" (VTuple (VFloat 1.0) (VFloat 4.0)) (0, 0.048)
        ]
    ]