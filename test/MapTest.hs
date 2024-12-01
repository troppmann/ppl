module MapTest
  ( tests,
  )
where

import Assert
import MaximumAPosteriori (map)
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

testMapExprWithName :: ExprString -> TestName -> QueryString -> Value -> TestTree
testMapExprWithName exprString testName queryString expectedValue = testCase testString $ do
  expr <- assertRight $ parseExpr exprString
  query <- assertRight $ parseQuery queryString
  (dim, value) <- assertRight $ MaximumAPosteriori.map expr query
  value @?= expectedValue
  where
    testString = shorter testName <> ":Query " <> shorter queryString

testMapExprFail :: ExprString -> QueryString -> ErrorString -> TestTree
testMapExprFail exprString queryString errorString = testCase testString $ do
  expr <- assertRight $ parseExpr exprString
  query <- assertRight $ parseQuery queryString
  MaximumAPosteriori.map expr query @?= Left errorString
  where
    testString = shorter exprString <> ":Query " <> shorter queryString <> ":Expected Fail"

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
          testMapExpr "if Uniform < 0.2 then 3 * 9 else 7" "_" (VFloat 7.0),
          testMapExpr "if Uniform < 0.3 then 5 else (if Uniform < 0.49 then 5 else 6)" "_" (VFloat 5),
          testMapExpr "if Uniform == 0.5 then 10 else 0" "_" (VFloat 0.0),
          testMapExpr "if Uniform != 3 then 10 else 0" "_" (VFloat 10.0),
          testMapExpr "if Uniform < 0.5 then 3 else Normal" "_" (VFloat 3.0),
          testMapExpr "if Uniform < 0.5 then Normal + 0.01 else Normal - 0.01" "_" (VFloat 0.0),
          testMapExpr "(Normal * 10 + 5, Normal * 5)" "_" (VTuple (VFloat 5) (VFloat 0)),
          testMapExpr "(Normal * 10 + 5, if Uniform > 0.7 then 8 else 0)" "_" (VTuple (VFloat 5) (VFloat 0)),
          testMapExpr "(if Uniform > 0.7 then 8 else 0, if Uniform > 0.7 then 8 else 0)" "_" (VTuple (VFloat 0) (VFloat 0)),
          testMapExpr "(Normal, if Uniform > 0.7 then 8 else 0, 8)" "_" (VTuple (VFloat 0) (VTuple (VFloat 0) (VFloat 8))),
          testMapExpr "if (Uniform < 0.4) || (Uniform < 0.4) then (5,5) else (Normal + 10, 10)" "_" (VTuple (VFloat 5) (VFloat 5)),
          testMapExpr "if Normal < 0.1 then (if Uniform == 0.4 then 5 else 8,5) else (10, 10)" "_" (VTuple (VFloat 8) (VFloat 5))
        ],
      testGroup
        "MAP with evidence p(q,e)"
        [ testMapExpr "(Normal, 3)" "(_, 3)" (VTuple (VFloat 0) (VFloat 3)),
          testMapExprFail "(Normal, 3)" "(_, 4)" "Value is not possible.",
          testMapExprFail "Uniform" "(4)" "Value is not possible.",
          testMapExpr "Uniform + 3.5" "(4)" (VFloat 4),
          testMapExpr "(Normal, Normal)" "(_, 3)" (VTuple (VFloat 0) (VFloat 3)),
          testMapExpr "(Normal, Normal)" "(3, _)" (VTuple (VFloat 3) (VFloat 0)),
          testMapExpr "(Normal, Normal + 7)" "(3, _)" (VTuple (VFloat 3) (VFloat 7)),
          testMapExpr "if Uniform < 0.5 then (Normal + 3, 10) else (Normal + 7, 0)" "(_, 10)" (VTuple (VFloat 3) (VFloat 10)),
          testMapExpr "if Uniform < 0.5 then (Normal + 3, 10) else (Normal + 7, 0)" "(3, _)" (VTuple (VFloat 3) (VFloat 10)),
          testMapExpr "if Uniform < 0.6 then Normal * 10 else 0.1 * Normal + 1 " "_" (VFloat 1.0),
          testMapExpr "if Uniform < 0.4 then (Normal + 3, 10) else (Normal + 7, 0)" "(3, _)" (VTuple (VFloat 3) (VFloat 0)),
          testMapExpr "if Uniform < 0.5 then (Normal + 3, 10) else (3, 0)" "(3, _)" (VTuple (VFloat 3) (VFloat 0))
        ]
    ]