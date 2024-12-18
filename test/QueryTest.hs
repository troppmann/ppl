module QueryTest
  ( tests,
  )
where

import Assert
import Parser
import Problems
import Query
import Representation
import Shorter
import Test.HUnit.Approx
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

type QueryString = String

testQueryExpr :: TestName -> QueryString -> DimensionalProbability -> TestTree
testQueryExpr testName = testQueryExprWithName testName testName

testQueryExprWithName :: String -> TestName -> QueryString -> DimensionalProbability -> TestTree
testQueryExprWithName exprString testName queryString (expectedDim, expectedProb) = testCase testString $ do
  expr <- assertRight $ parseExpr exprString
  query <- assertRight $ parseQuery queryString
  (dim, prob) <- assertRight $ qInterpret expr query
  dim @?= expectedDim
  assertApproxEqual "" defaultErrorMargin expectedProb prob
  where
    testString = shorter testName <> ":Query " <> shorter queryString

testQueryExprFail :: TestName -> QueryString -> String -> TestTree
testQueryExprFail exprString queryString errorString = testCase msg $ do
  expr <- assertRight $ parseExpr exprString
  query <- assertRight $ parseQuery queryString
  error <- assertLeft $ qInterpret expr query
  error @?= errorString
  where
    msg = shorter exprString <> ":Expected Error"

tests =
  testGroup
    "Query"
    [ testGroup
        "Basic"
        [ testQueryExpr "Uniform" "(_ < 0.3)" (0, 0.3),
          testQueryExpr "Uniform" "(0.3)" (1, 1.0),
          testQueryExpr "Uniform" "_" (0, 1.0),
          testQueryExpr "Uniform" "(_ >= 0.3)" (0, 0.7),
          testQueryExpr "Uniform" "2 <= _" (0, 0.0),
          testQueryExprFail "Uniform" "(_, _ >= 2.0)" "Can't interpret singular value expression with a tuple query.",
          testQueryExpr "Uniform * 3" "(_ >= 2.0)" (0, 1 / 3),
          testQueryExpr "if Uniform < 0.5 then Uniform * 3 else Uniform" "(_ >= 2.0)" (0, 0.5 * (1 / 3)),
          testQueryExpr "(Uniform * 3, 2.0 - 20)" "(_ >= 2.0, _)" (0, 1 / 3),
          testQueryExpr "(Uniform, Uniform)" "(_ <= 0.2, _ <= 0.2)" (0, 0.2 * 0.2),
          testQueryExpr "(4 * Uniform, Uniform)" "(2.1, _)" (1, 0.25),
          testQueryExprFail "(4 * Uniform, Uniform)" "(2.1)" "Can't interpret a tuple expression with a singular expression.",
          testQueryExpr "(4 * Uniform, 8 * Uniform)" "(2.1, _ >= 4.0)" (1, 0.25 * 0.5),
          testQueryExpr "(4 * Uniform, 8 * Uniform, Normal)" "(2.1, _ >= 4.0, 0.0)" (2, 0.25 * 0.5 * 0.3989),
          testQueryExpr "((4 * Uniform, 8 * Uniform), Normal)" "((2.1, _ >= 4.0), 0.0)" (2, 0.25 * 0.5 * 0.3989)
        ],
      testGroup
        "ControlFlow"
        [ testQueryExpr "if Uniform < 0.5 then (0, Uniform * 2) else (1, Uniform * 2 + 1)" "(_, 1.5)" (1, 0.5),
          testQueryExpr "if Uniform < 0.5 then (Uniform, Uniform * 2) else (Uniform * 3, Uniform * 2 + 1)" "(_ >= 0.5, 1.5)" (1, 1 / 3)
        ],
      testGroup
        "Problems"
        [ testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" "(_, 0.5)" (1, 0.5 * 0.99 * 0.25 + 0.5 * 0.99 * 0.1),
          testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" "(_, 4.0)" (0, 0.5 * 0.01),
          testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" "(_, 9.5)" (1, 0.5 * 0.99 * 0.1),
          testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" "(_, 10.0)" (0, 0.5 * 0.01),
          testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" "(_, 12.0)" (0, 0.0),
          testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" "(_, _ > 2.0)" (0, 0.5 * 0.99 * 0.5 + 0.5 * 0.01 + 0.5 * 0.99 * 0.8 + 0.5 * 0.01),
          testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" "(_, _ > 6.0)" (0, 0.5 * 0.99 * 0.4 + 0.5 * 0.01),
          testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" "(1, _)" (0, 0.5),
          testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" "(1, _ > 6.0)" (0, 0.5 * 0.99 * 0.4 + 0.5 * 0.01),
          testQueryExprWithName add2Uniform "add2Uniform(..)" "(_ < 1)" (0, 0.5),
          testQueryExprWithName add2Uniform "add2Uniform(..)" "(_ < 1.5)" (0, 0.5 + 0.75 * 0.5)
        ]
    ]