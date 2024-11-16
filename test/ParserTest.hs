module ParserTest
  ( tests,
  )
where

import Assert
import Parser
import Problems
import Query
import Representation
import Shorter
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

testParseExpr :: String -> Expr -> TestTree
testParseExpr exprString = testParseExprWithName exprString exprString

testParseExprWithName :: String -> TestName -> Expr -> TestTree
testParseExprWithName exprString testName expectedExpr = testCase msg $ do
  expr <- assertRight $ parseExpr exprString
  expr @?= expectedExpr
  where
    msg = shorter testName

testParseExprFail :: TestName -> String -> TestTree
testParseExprFail exprString errorString = testCase msg $ do
  error <- assertLeft $ parseExpr exprString
  error @?= errorString
  where
    msg = shorter exprString <> ":Expected Error"

testParseQuery :: String -> QueryType -> TestTree
testParseQuery queryString expectedQuery = testCase msg $ do
  query <- assertRight $ parseQuery queryString
  query @?= expectedQuery
  where
    msg = shorter queryString

tests =
  testGroup
    "Parser"
    [ testGroup
        "Basic"
        [ testParseExpr "True" (Const $ VBool True),
          testParseExpr "False" (Const $ VBool False),
          testParseExpr "3.4" (Const $ VFloat 3.4),
          testParseExpr "-3.4" (Const $ VFloat (-3.4)),
          testParseExpr "Normal" Normal,
          testParseExpr "Uniform" Uniform,
          testParseExpr "(Uniform,Normal)" $ CreateTuple Uniform Normal,
          testParseExpr "(Uniform,3.4)" $ CreateTuple Uniform (Const $ VFloat 3.4),
          testParseExpr "(Uniform,(3.4, Normal))" $ CreateTuple Uniform (CreateTuple (Const $ VFloat 3.4) Normal),
          testParseExpr "(Uniform, 3.4, Normal)" $ CreateTuple Uniform (CreateTuple (Const $ VFloat 3.4) Normal),
          testParseExpr "(Uniform, (3.4), Normal)" $ CreateTuple Uniform (CreateTuple (Const $ VFloat 3.4) Normal),
          testParseExpr "((Normal, (Uniform, True)),(3.4, Normal))" $ CreateTuple (CreateTuple Normal (CreateTuple Uniform (Const $ VBool True))) (CreateTuple (Const $ VFloat 3.4) Normal),
          testParseExpr "(Uniform, 3.4 * Uniform, Normal)" $ CreateTuple Uniform (CreateTuple (Multiply (Const $ VFloat 3.4) Uniform) Normal),
          testParseExpr "(Uniform, (if True then 2 else 1), Normal)" $ CreateTuple Uniform (CreateTuple (IfElseThen (Const $ VBool True) (Const $ VFloat 2) (Const $ VFloat 1)) Normal),
          testParseExpr "(Uniform, if True then 2 else 1, Normal)" $ CreateTuple Uniform (CreateTuple (IfElseThen (Const $ VBool True) (Const $ VFloat 2) (Const $ VFloat 1)) Normal),
          testParseExpr "(Uniform, 3 * if True then 2 else 1, Normal)" $ CreateTuple Uniform (CreateTuple (Multiply (Const $ VFloat 3.0) (IfElseThen (Const $ VBool True) (Const $ VFloat 2) (Const $ VFloat 1))) Normal),
          testParseExpr "(Uniform, Uniform, Uniform, Uniform)" $ CreateTuple Uniform (CreateTuple Uniform (CreateTuple Uniform Uniform)),
          testParseExpr "5.0 + Normal" $ Plus (Const (VFloat 5.0)) Normal,
          testParseExpr "5.0 - Normal" $ Subtract (Const (VFloat 5.0)) Normal,
          testParseExpr "5.0 * 3.4" $ Multiply (Const (VFloat 5.0)) (Const (VFloat 3.4)),
          testParseExpr "5.0 / Normal" $ Divide (Const (VFloat 5.0)) Normal,
          testParseExpr "5.0 < Normal" $ LessThan (Const (VFloat 5.0)) Normal,
          testParseExpr "5.0 <= Normal" $ LessThanOrEqual (Const (VFloat 5.0)) Normal,
          testParseExpr "5.0 > Normal" $ GreaterThan (Const (VFloat 5.0)) Normal,
          testParseExpr "5.0 >= Normal" $ GreaterThanOrEqual (Const (VFloat 5.0)) Normal,
          testParseExpr "5.0 == Normal" $ Equal (Const (VFloat 5.0)) Normal,
          testParseExpr "5.0 != Normal" $ Unequal (Const (VFloat 5.0)) Normal,
          testParseExpr "True != False" $ Unequal (Const (VBool True)) (Const (VBool False)),
          testParseExpr "True && False" $ And (Const (VBool True)) (Const (VBool False)),
          testParseExpr "True || False" $ Or (Const (VBool True)) (Const (VBool False)),
          testParseExpr "! False" $ Not (Const (VBool False)),
          testParseExpr "True && !False && False" $ And (And (Const (VBool True)) (Not (Const (VBool False)))) (Const (VBool False))
        ],
      testGroup
        "Padding"
        [ testParseExprFail "- 3.4" "Error: Expected Value got Operator '-'",
          testParseExpr "!False" $ Not (Const (VBool False)),
          testParseExpr "(False)" $ Const (VBool False),
          testParseExpr "(False" $ Const (VBool False),
          testParseExpr "(True,(False, False" $ CreateTuple (Const (VBool True)) (CreateTuple (Const (VBool False)) (Const (VBool False))),
          testParseExpr "!(3.4)" $ Not (Const (VFloat 3.4))
        ],
      testGroup
        "ControlFlow"
        [ testParseExpr "if Uniform > 0.5 then Normal else Normal * 6.7" $ IfElseThen (GreaterThan Uniform (Const (VFloat 0.5))) Normal (Multiply Normal (Const (VFloat 6.7))),
          testParseExpr "3 * 4 + 12" (Plus (Multiply (Const (VFloat 3)) (Const (VFloat 4))) (Const (VFloat 12))),
          testParseExpr "3 + 4 * 12" (Multiply (Plus (Const (VFloat 3)) (Const (VFloat 4))) (Const (VFloat 12))),
          testParseExpr "3 + (4 * 12)" (Plus (Const (VFloat 3)) (Multiply (Const (VFloat 4)) (Const (VFloat 12)))),
          testParseExpr "3 + ((if Uniform > 0.5 then Normal else Normal * 6.7) * 12)" (Plus (Const (VFloat 3)) (Multiply (IfElseThen (GreaterThan Uniform (Const (VFloat 0.5))) Normal (Multiply Normal (Const (VFloat 6.7)))) (Const (VFloat 12)))),
          testParseExpr "if Uniform < 0.5 then (0, 4) else (1, 10)" (IfElseThen (LessThan Uniform (Const $ VFloat 0.5)) (CreateTuple (Const $ VFloat 0) (Const $ VFloat 4)) (CreateTuple (Const $ VFloat 1) (Const $ VFloat 10))),
          testParseExpr "(if Uniform then Uniform else Uniform) * Uniform" (Multiply (IfElseThen Uniform Uniform Uniform) Uniform),
          testParseExpr "if Uniform then (if Uniform then Uniform else Uniform) else Uniform" (IfElseThen Uniform (IfElseThen Uniform Uniform Uniform) Uniform),
          testParseExpr "if Uniform then (3 * if Uniform then Uniform else Uniform) else Uniform" (IfElseThen Uniform (Multiply (Const $ VFloat 3) (IfElseThen Uniform Uniform Uniform)) Uniform),
          testParseExprWithName indiaGpaProblem "IndiaGpaProblem(..)" (IfElseThen (LessThan Uniform (Const $ VFloat 0.5)) (CreateTuple (Const $ VFloat 0) (IfElseThen (LessThan Uniform (Const $ VFloat 0.01)) (Const $ VFloat 4) (Multiply Uniform (Const $ VFloat 4)))) (CreateTuple (Const $ VFloat 1) (IfElseThen (LessThan Uniform (Const $ VFloat 0.01)) (Const $ VFloat 10) (Multiply Uniform (Const $ VFloat 10)))))
        ],
      testGroup
        "Query"
        [ testParseQuery "_" QAny,
          testParseQuery "True" (QIs True),
          testParseQuery "False" (QIs False),
          testParseQuery "3.1" (QAt 3.1),
          testParseQuery "_ > 3" (QGt 3),
          testParseQuery "3 < _" (QGt 3),
          testParseQuery "3.1 >= _" (QLe 3.1),
          testParseQuery "_ <= 3.1" (QLe 3.1),
          testParseQuery "_ < 3" (QLt 3),
          testParseQuery "3 > _" (QLt 3),
          testParseQuery "3.1 <= _" (QGe 3.1),
          testParseQuery "_ >= 3.1" (QGe 3.1),
          testParseQuery "_, _" (QTuple QAny QAny),
          testParseQuery "_, 3.2" (QTuple QAny (QAt 3.2)),
          testParseQuery "_ > 3, _ <= 2" (QTuple (QGt 3) (QLe 2)),
          testParseQuery "_ > 3, _ <= 2, _" (QTuple (QGt 3) (QTuple (QLe 2) QAny))
        ]
    ]
