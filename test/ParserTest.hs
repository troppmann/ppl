module ParserTest
  ( tests,
  )
where

import Assert
import Parser
import Representation
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

-- testCaseParseExpr :: String -> Expr -> Assertion
testCaseParseExpr :: TestName -> Expr -> TestTree
testCaseParseExpr exprString expectedExpr = testCase exprString $ do
  expr <- assertRight $ parseExpr exprString
  expr @?= expectedExpr

testCaseParseExprError :: TestName -> String -> TestTree
testCaseParseExprError exprString errorString = testCase msg $ do
  error <- assertLeft $ parseExpr exprString
  error @?= errorString
  where
    msg = exprString <> ":Expected Error"

tests =
  testGroup
    "Parser"
    [ testGroup
        "Basic"
        [ testCaseParseExpr "True" (Const $ VBool True),
          testCaseParseExpr "False" (Const $ VBool False),
          testCaseParseExpr "3.4" (Const $ VFloat 3.4),
          testCaseParseExpr "-3.4" (Const $ VFloat (-3.4)),
          testCaseParseExpr "Normal" Normal,
          testCaseParseExpr "Uniform" Uniform,
          testCaseParseExpr "(Uniform,Normal)" $ CreateTuple Uniform Normal,
          testCaseParseExpr "(Uniform,3.4)" $ CreateTuple Uniform (Const $ VFloat 3.4),
          testCaseParseExpr "(Uniform,(3.4, Normal))" $ CreateTuple Uniform (CreateTuple (Const $ VFloat 3.4) Normal),
          testCaseParseExpr "(Uniform, 3.4, Normal)" $ CreateTuple Uniform (CreateTuple (Const $ VFloat 3.4) Normal),
          testCaseParseExpr "(Uniform, (3.4), Normal)" $ CreateTuple Uniform (CreateTuple (Const $ VFloat 3.4) Normal),
          testCaseParseExpr "((Normal, (Uniform, True)),(3.4, Normal))" $ CreateTuple (CreateTuple Normal (CreateTuple Uniform (Const $ VBool True))) (CreateTuple (Const $ VFloat 3.4) Normal),
          testCaseParseExpr "(Uniform, 3.4 * Uniform, Normal)" $ CreateTuple Uniform (CreateTuple (Multiply (Const $ VFloat 3.4) Uniform) Normal),
          testCaseParseExpr "(Uniform, (if True then 2 else 1), Normal)" $ CreateTuple Uniform (CreateTuple (IfElseThen (Const $ VBool True) (Const $ VFloat 2) (Const $ VFloat 1)) Normal),
          testCaseParseExpr "(Uniform, if True then 2 else 1, Normal)" $ CreateTuple Uniform (CreateTuple (IfElseThen (Const $ VBool True) (Const $ VFloat 2) (Const $ VFloat 1)) Normal),
          testCaseParseExpr "(Uniform, 3 * if True then 2 else 1, Normal)" $ CreateTuple Uniform (CreateTuple (Multiply (Const $ VFloat 3.0) (IfElseThen (Const $ VBool True) (Const $ VFloat 2) (Const $ VFloat 1))) Normal),
          testCaseParseExpr "(Uniform, Uniform, Uniform, Uniform)" $ CreateTuple Uniform (CreateTuple Uniform (CreateTuple Uniform Uniform)),
          testCaseParseExpr "5.0 + Normal" $ Plus (Const (VFloat 5.0)) Normal,
          testCaseParseExpr "5.0 - Normal" $ Subtract (Const (VFloat 5.0)) Normal,
          testCaseParseExpr "5.0 * 3.4" $ Multiply (Const (VFloat 5.0)) (Const (VFloat 3.4)),
          testCaseParseExpr "5.0 / Normal" $ Divide (Const (VFloat 5.0)) Normal,
          testCaseParseExpr "5.0 < Normal" $ LessThan (Const (VFloat 5.0)) Normal,
          testCaseParseExpr "5.0 <= Normal" $ LessThanOrEqual (Const (VFloat 5.0)) Normal,
          testCaseParseExpr "5.0 > Normal" $ GreaterThan (Const (VFloat 5.0)) Normal,
          testCaseParseExpr "5.0 >= Normal" $ GreaterThanOrEqual (Const (VFloat 5.0)) Normal,
          testCaseParseExpr "5.0 == Normal" $ Equal (Const (VFloat 5.0)) Normal,
          testCaseParseExpr "5.0 != Normal" $ Unequal (Const (VFloat 5.0)) Normal,
          testCaseParseExpr "True != False" $ Unequal (Const (VBool True)) (Const (VBool False)),
          testCaseParseExpr "True && False" $ And (Const (VBool True)) (Const (VBool False)),
          testCaseParseExpr "True || False" $ Or (Const (VBool True)) (Const (VBool False)),
          testCaseParseExpr "! False" $ Not (Const (VBool False)),
          testCaseParseExpr "True && !False && False" $ And (And (Const (VBool True)) (Not (Const (VBool False)))) (Const (VBool False))
        ],
      testGroup
        "Padding"
        [ testCaseParseExprError "- 3.4" "Error: Expected Value got Operator '-'",
          testCaseParseExpr "!False" $ Not (Const (VBool False)),
          testCaseParseExpr "(False)" $ Const (VBool False),
          testCaseParseExpr "!(3.4)" $ Not (Const (VFloat 3.4))
        ],
      testGroup
        "ControlFlow"
        [ testCaseParseExpr "if Uniform > 0.5 then Normal else Normal * 6.7" $ IfElseThen (GreaterThan Uniform (Const (VFloat 0.5))) Normal (Multiply Normal (Const (VFloat 6.7))),
          testCaseParseExpr "3 * 4 + 12" (Plus (Multiply (Const (VFloat 3)) (Const (VFloat 4))) (Const (VFloat 12))),
          testCaseParseExpr "3 + 4 * 12" (Multiply (Plus (Const (VFloat 3)) (Const (VFloat 4))) (Const (VFloat 12))),
          testCaseParseExpr "3 + (4 * 12)" (Plus (Const (VFloat 3)) (Multiply (Const (VFloat 4)) (Const (VFloat 12)))),
          testCaseParseExpr "3 + ((if Uniform > 0.5 then Normal else Normal * 6.7) * 12)" (Plus (Const (VFloat 3)) (Multiply (IfElseThen (GreaterThan Uniform (Const (VFloat 0.5))) Normal (Multiply Normal (Const (VFloat 6.7)))) (Const (VFloat 12))))
        ]
    ]