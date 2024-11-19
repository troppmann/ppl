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

testMapExpr :: TestName -> QueryString -> Value -> TestTree
testMapExpr testName = testMapExprWithName testName testName

testMapExprWithName :: ExprString -> TestName -> QueryString -> Value -> TestTree
testMapExprWithName exprString testName queryString expectedValue = testCase testString $ do
  expr <- assertRight $ parseExpr exprString
  query <- assertRight $ parseQuery queryString
  value <- assertRight $ MaximumAPosteriori.map expr query
  value @?= expectedValue
  where
    testString = shorter testName <> ":Query " <> shorter queryString

tests =
  testGroup
    "MaximumAPosteriori"
    [ testMapExpr "3" "_" (VFloat 3.0),
      testMapExpr "Normal" "_" (VFloat 0.0),
      testMapExpr "Uniform" "_" (VFloat 0.5),
      testMapExpr "Normal * 3 + 7" "_" (VFloat 7.0),
      testMapExpr "(Normal + 7) * 3" "_" (VFloat 21.0),
      testMapExpr "((Normal + 7) * 3 - 1) / 2" "_" (VFloat 10.0),
      testMapExpr "if Uniform < 0.2 then 3 * 9 else 7" "_" (VFloat 7.0),
      testMapExpr "if Uniform == 0.5 then 10 else 0" "_" (VFloat 0.0),
      testMapExpr "if Uniform == 3 then 10 else 0" "_" (VFloat 0.0),
      testMapExpr "(Normal * 10 + 5, Normal * 5)" "_" (VTuple (VFloat 5) (VFloat 0)),
      testMapExpr "(Normal * 10 + 5, if Uniform > 0.7 then 8 else 0)" "_" (VTuple (VFloat 5) (VFloat 0)),
      testMapExpr "(if Uniform > 0.7 then 8 else 0, if Uniform > 0.7 then 8 else 0)" "_" (VTuple (VFloat 0) (VFloat 0)),
      testMapExpr "(Normal, if Uniform > 0.7 then 8 else 0, 8)" "_" (VTuple (VFloat 0) (VTuple (VFloat 0) (VFloat 8))),
      testMapExpr "if (Uniform < 0.4) || (Uniform < 0.4) then (5,5) else (Normal + 10, 10)" "_" (VTuple (VFloat 5) (VFloat 5)),
      testMapExpr "if Normal < 0.1 then (if Uniform == 0.4 then 5 else 8,5) else (10, 10)" "_" (VTuple (VFloat 8) (VFloat 5))
    ]