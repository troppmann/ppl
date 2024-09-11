module SampleTest
  ( tests,
  )
where

import Test.Tasty
import Test.Tasty.HUnit

tests =
  testGroup
    "Sample"
    [ testCase "Const" $ 3 @?= (2 + 1),
      testCase "Const2" $ 3 @?= (2 + 6)
    ]
