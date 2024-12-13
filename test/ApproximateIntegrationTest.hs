module ApproximateIntegrationTest
  ( tests,
  )
where

import ApproximateIntegration
import Assert
import Parser
import Representation
import Test.HUnit.Approx
import Test.Tasty
import Test.Tasty.HUnit

testApproxFloatPdf :: TestName -> LinearSpacing -> TestTree
testApproxFloatPdf exprString linSpace = testCase testString $ do
  expr <- assertRight $ parseExpr exprString
  let area = approxExpr linSpace expr
  assertApproxEqual "" defaultErrorMargin 1 area
  where
    testString = exprString

tests =
  testGroup
    "ApproximateIntegration"
    [ testApproxFloatPdf "Uniform" linSpace,
      testApproxFloatPdf "Uniform * 5" linSpace,
      testApproxFloatPdf "Uniform * 5 - 2" linSpace,
      -- testApproxFloatPdf "Uniform ** 2" positiveSpace,
      -- testApproxFloatPdf "Normal ** 2" accurateSpace,
      -- testApproxFloatPdf "Uniform ** 0.5" positiveSpace,
      testApproxFloatPdf "Normal" linSpace,
      testApproxFloatPdf "Normal * 5" linSpace,
      testApproxFloatPdf "Normal * 3 - 2" linSpace,
      testApproxFloatPdf "if Uniform > 0.5 then Uniform else Normal" linSpace,
      testApproxFloatPdf "if Uniform > 0.1 then Uniform + 5 else Uniform - 5" linSpace,
      testApproxFloatPdf "3 + ((if Uniform > 0.5 then Normal else Normal * 6.7) * 0.5)" linSpace
    ]
  where
    linSpace = LinearSpacing {start = -20, end = 20, stepWidth = 0.01}
    positiveSpace = LinearSpacing {start = 0.0000001, end = 1, stepWidth = 0.0000001}
    accurateSpace = LinearSpacing {start = -20, end = 20, stepWidth = 0.00001}