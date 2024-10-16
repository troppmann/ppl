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

approxFloatPdf :: TestName -> LinearSpacing -> TestTree
approxFloatPdf exprString linSpace = testCase testString $ do
  expr <- assertRight $ parseExpr exprString
  let area = approxExpr linSpace expr
  assertApproxEqual "" defaultErrorMargin 1 area
  where
    testString = exprString

tests =
  testGroup
    "ApproximateIntegration"
    [ approxFloatPdf "Uniform" linSpace,
      approxFloatPdf "Uniform * 5" linSpace,
      approxFloatPdf "Uniform * 5 - 2" linSpace,
      approxFloatPdf "Normal" linSpace,
      approxFloatPdf "Normal * 5" linSpace,
      approxFloatPdf "Normal * 3 - 2" linSpace,
      approxFloatPdf "if Uniform > 0.5 then Uniform else Normal" linSpace,
      approxFloatPdf "if Uniform > 0.1 then Uniform + 5 else Uniform - 5" linSpace,
      approxFloatPdf "3 + ((if Uniform > 0.5 then Normal else Normal * 6.7) * 0.5)" linSpace
    ]
  where
    linSpace = LinearSpacing {start = -20, end = 20, stepWidth = 0.01}