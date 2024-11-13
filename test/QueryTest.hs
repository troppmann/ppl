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

testQueryExpr :: TestName -> QueryType -> DimensionalProbability -> TestTree
testQueryExpr testName = testQueryExprWithName testName testName

testQueryExprWithName :: String -> TestName -> QueryType -> DimensionalProbability -> TestTree
testQueryExprWithName exprString testName query (expectedDim, expectedProb) = testCase testString $ do
  expr <- assertRight $ parseExpr exprString
  (dim, prob) <- assertRight $ qInterpret expr query
  dim @?= expectedDim
  assertApproxEqual "" defaultErrorMargin expectedProb prob
  where
    testString = shorter testName <> ":Query " <> shorter query

testQueryExprFail :: TestName -> QueryType -> String -> TestTree
testQueryExprFail exprString query errorString = testCase msg $ do
  expr <- assertRight $ parseExpr exprString
  error <- assertLeft $ qInterpret expr query
  error @?= errorString
  where
    msg = shorter exprString <> ":Expected Error"

tests =
  testGroup
    "Query"
    [ testGroup
        "Basic"
        [ testQueryExpr "Uniform" (QLe 0.3) (0, 0.3),
          testQueryExpr "Uniform" (QAt 0.3) (1, 1.0),
          testQueryExpr "Uniform" QAny (0, 1.0),
          testQueryExpr "Uniform" (QGe 0.3) (0, 0.7),
          testQueryExpr "Uniform" (QGe 2.0) (0, 0.0),
          testQueryExprFail "Uniform" (QTuple QAny $ QGe 2.0) "Can't interpret singular value expression with a tuple query.",
          testQueryExpr "Uniform * 3" (QGe 2.0) (0, 1 / 3),
          testQueryExpr "if Uniform < 0.5 then Uniform * 3 else Uniform" (QGe 2.0) (0, 0.5 * (1 / 3)),
          testQueryExpr "(Uniform * 3, 2.0 - 20)" (QTuple (QGe 2.0) QAny) (0, 1 / 3),
          testQueryExpr "(Uniform, Uniform)" (QTuple (QLt 0.2) (QLt 0.2)) (0, 0.2 * 0.2),
          testQueryExpr "(4 * Uniform, Uniform)" (QTuple (QAt 2.1) QAny) (1, 0.25),
          testQueryExprFail "(4 * Uniform, Uniform)" (QAt 2.1) "Can't interpret a tuple expression with a singular expression.",
          testQueryExpr "(4 * Uniform, 8 * Uniform)" (QTuple (QAt 2.1) (QGt 4.0)) (1, 0.25 * 0.5),
          testQueryExpr "(4 * Uniform, 8 * Uniform, Normal)" (QTuple (QAt 2.1) (QTuple (QGt 4.0) (QAt 0.0))) (2, 0.25 * 0.5 * 0.3989)
        ],
      testGroup
        "ControlFlow"
        [ testQueryExpr "if Uniform < 0.5 then (0, Uniform * 2) else (1, Uniform * 2 + 1)" (QTuple QAny (QAt 1.5)) (1, 0.5),
          testQueryExpr "if Uniform < 0.5 then (Uniform, Uniform * 2) else (Uniform * 3, Uniform * 2 + 1)" (QTuple (QGt 0.5) (QAt 1.5)) (1, 1 / 3)
        ],
      testGroup
        "Problems"
        [ testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" (QTuple QAny (QAt 0.5)) (1, 0.5 * 0.99 * 0.25 + 0.5 * 0.99 * 0.1),
          testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" (QTuple QAny (QAt 4.0)) (0, 0.5 * 0.01),
          testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" (QTuple QAny (QAt 9.5)) (1, 0.5 * 0.99 * 0.1),
          testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" (QTuple QAny (QAt 10.0)) (0, 0.5 * 0.01),
          testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" (QTuple QAny (QAt 12.0)) (0, 0.0),
          testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" (QTuple QAny (QGt 2.0)) (0, 0.5 * 0.99 * 0.5 + 0.5 * 0.01 + 0.5 * 0.99 * 0.8 + 0.5 * 0.01),
          testQueryExprWithName indiaGpaProblem "IndiaGpaProblem(..)" (QTuple QAny (QGt 6.0)) (0, 0.5 * 0.99 * 0.4 + 0.5 * 0.01)
        ]
    ]