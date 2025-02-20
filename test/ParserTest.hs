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
import Parser.String (ErrorString)

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

testParseProgram:: String -> Either ErrorString Program -> TestTree
testParseProgram programString expected = testCase msg $ do
  expected @?= parseProgram programString
  where
    msg = shorter programString
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
          testParseExpr "(Uniform, (if True then 2 else 1), Normal)" $ CreateTuple Uniform (CreateTuple (IfThenElse (Const $ VBool True) (Const $ VFloat 2) (Const $ VFloat 1)) Normal),
          testParseExpr "(Uniform, if True then 2 else 1, Normal)" $ CreateTuple Uniform (CreateTuple (IfThenElse (Const $ VBool True) (Const $ VFloat 2) (Const $ VFloat 1)) Normal),
          testParseExpr "(Uniform, 3 * if True then 2 else 1, Normal)" $ CreateTuple Uniform (CreateTuple (Multiply (Const $ VFloat 3.0) (IfThenElse (Const $ VBool True) (Const $ VFloat 2) (Const $ VFloat 1))) Normal),
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
          testParseExpr "Uniform ** 3" $ Exponent Uniform (Const (VFloat 3.0)),
          testParseExpr "abs Uniform ** 3" $ Exponent (Abs Uniform) (Const (VFloat 3.0)),
          testParseExpr "! False" $ Not (Const (VBool False)),
          testParseExpr "True && !False && False" $ And (And (Const (VBool True)) (Not (Const (VBool False)))) (Const (VBool False))
        ],
      testGroup
        "Padding"
        [ testParseExpr "- 3.4" $ Const (VFloat (-3.4)),
          testParseExpr "-3.4" $ Const (VFloat (-3.4)),
          testParseExpr "3-3.4" $ Subtract (Const $ VFloat 3.0) (Const $ VFloat 3.4),
          testParseExpr "3*-3.4" $ Multiply (Const $ VFloat 3.0) (Const $ VFloat (-3.4)),
          testParseExpr "Uniform-3.4" $ Subtract Uniform (Const $ VFloat 3.4),
          testParseExpr "3.4-Uniform" $ Subtract (Const $ VFloat 3.4) Uniform,
          testParseExpr "3.4*-Uniform" $ Multiply (Const $ VFloat 3.4) (Multiply (Const $ VFloat (-1)) Uniform),
          testParseExpr "-Uniform" $ Multiply (Const $ VFloat (-1)) Uniform,
          testParseExpr "!False" $ Not (Const (VBool False)),
          testParseExpr "(False)" $ Const (VBool False),
          testParseExpr "(False" $ Const (VBool False),
          testParseExpr "Uniform*3" $ Multiply Uniform (Const $ VFloat 3.0),
          testParseExpr "Uniform==3" $ Equal Uniform (Const $ VFloat 3.0),
          testParseExpr "Uniform!=3" $ Unequal Uniform (Const $ VFloat 3.0),
          testParseExpr "Uniform<=Uniform" $ LessThanOrEqual Uniform Uniform,
          testParseExpr "Uniform<Uniform" $ LessThan Uniform Uniform,
          testParseExpr "Uniform>=Uniform" $ GreaterThanOrEqual Uniform Uniform,
          testParseExpr "Uniform>-Uniform" $ GreaterThan Uniform (Multiply (Const $ VFloat (-1)) Uniform),
          testParseExpr "(True,(False, False" $ CreateTuple (Const (VBool True)) (CreateTuple (Const (VBool False)) (Const (VBool False))),
          testParseExpr "!(3.4)" $ Not (Const (VFloat 3.4))
        ],
      testGroup
        "ControlFlow"
        [ testParseExpr "if Uniform > 0.5 then Normal else Normal * 6.7" $ IfThenElse (GreaterThan Uniform (Const (VFloat 0.5))) Normal (Multiply Normal (Const (VFloat 6.7))),
          testParseExpr "3 * 4 + 12" (Plus (Multiply (Const (VFloat 3)) (Const (VFloat 4))) (Const (VFloat 12))),
          testParseExpr "3 + 4 * 12" (Multiply (Plus (Const (VFloat 3)) (Const (VFloat 4))) (Const (VFloat 12))),
          testParseExpr "3 + (4 * 12)" (Plus (Const (VFloat 3)) (Multiply (Const (VFloat 4)) (Const (VFloat 12)))),
          testParseExpr "3 + ((if Uniform > 0.5 then Normal else Normal * 6.7) * 12)" (Plus (Const (VFloat 3)) (Multiply (IfThenElse (GreaterThan Uniform (Const (VFloat 0.5))) Normal (Multiply Normal (Const (VFloat 6.7)))) (Const (VFloat 12)))),
          testParseExpr "if Uniform < 0.5 then (0, 4) else (1, 10)" (IfThenElse (LessThan Uniform (Const $ VFloat 0.5)) (CreateTuple (Const $ VFloat 0) (Const $ VFloat 4)) (CreateTuple (Const $ VFloat 1) (Const $ VFloat 10))),
          testParseExpr "(if Uniform then Uniform else Uniform) * Uniform" (Multiply (IfThenElse Uniform Uniform Uniform) Uniform),
          testParseExpr "if Uniform then (if Uniform then Uniform else Uniform) else Uniform" (IfThenElse Uniform (IfThenElse Uniform Uniform Uniform) Uniform),
          testParseExpr "if Uniform then (3 * if Uniform then Uniform else Uniform) else Uniform" (IfThenElse Uniform (Multiply (Const $ VFloat 3) (IfThenElse Uniform Uniform Uniform)) Uniform),
          testParseExprWithName indiaGpaProblem "IndiaGpaProblem(..)" (IfThenElse (LessThan Uniform (Const $ VFloat 0.5)) (CreateTuple (Const $ VFloat 0) (IfThenElse (LessThan Uniform (Const $ VFloat 0.01)) (Const $ VFloat 4) (Multiply Uniform (Const $ VFloat 4)))) (CreateTuple (Const $ VFloat 1) (IfThenElse (LessThan Uniform (Const $ VFloat 0.01)) (Const $ VFloat 10) (Multiply Uniform (Const $ VFloat 10)))))
        ],
      testGroup
        "Query"
        [ testParseQuery "_" QAny,
          testParseQuery "True" (QBool NormalMode True),
          testParseQuery "False" (QBool NormalMode False),
          testParseQuery "3.1" (QFloat NormalMode 3.1),
          testParseQuery "_ > 3" (QGt NormalMode 3),
          testParseQuery "3 < _" (QGt NormalMode 3),
          testParseQuery "3.1 >= _" (QLe NormalMode 3.1),
          testParseQuery "_ <= 3.1" (QLe NormalMode 3.1),
          testParseQuery "_ < 3" (QLt NormalMode 3),
          testParseQuery "3 > _" (QLt NormalMode 3),
          testParseQuery "3.1 <= _" (QGe NormalMode 3.1),
          testParseQuery "_ >= 3.1" (QGe NormalMode 3.1),
          testParseQuery "_, _" (QTuple QAny QAny),
          testParseQuery "_, 3.2" (QTuple QAny (QFloat NormalMode 3.2)),
          testParseQuery "_ > 3, _ <= 2" (QTuple (QGt NormalMode 3) (QLe NormalMode 2)),
          testParseQuery "_ > 3, _ <= 2, _" (QTuple (QGt NormalMode 3) (QTuple (QLe NormalMode 2) QAny)),
          testParseQuery "(_ < 3)" (QLt NormalMode 3),
          testParseQuery "(_ > 3, _ <= 2, _)" (QTuple (QGt NormalMode 3) (QTuple (QLe NormalMode 2) QAny)),
          testParseQuery "(_ > 3, (_ <= 2, _))" (QTuple (QGt NormalMode 3) (QTuple (QLe NormalMode 2) QAny)),
          testParseQuery "(_ > 3, (_ <= 2), _)" (QTuple (QGt NormalMode 3) (QTuple (QLe NormalMode 2) QAny)),
          testParseQuery "((_ > 3, _ <= 2), _)" (QTuple (QTuple (QGt NormalMode 3) (QLe NormalMode 2)) QAny),
          testParseQuery "((_ > 3, _ <= 2), (_, _))" (QTuple (QTuple (QGt NormalMode 3) (QLe NormalMode 2)) (QTuple QAny QAny))
        ],
      testGroup
        "FnCall"
        [ testParseExpr "3 + 5 * main" (Multiply (Plus (Const (VFloat 3.0)) (Const (VFloat 5.0))) (FnCall "main" [])),
          testParseExpr "8.0 * dice 3" (Multiply (Const (VFloat 8.0)) (FnCall "dice" [Const (VFloat 3.0)])),
          testParseExpr "randomFunc 3 4 " (FnCall "randomFunc" [Const (VFloat 3.0), Const (VFloat 4.0)]),
          testParseExpr "randomFunc (3 + 4) 2 " (FnCall "randomFunc" [Plus (Const (VFloat 3.0)) (Const (VFloat 4.0)), Const (VFloat 2.0)]),
          testParseExpr "3 * randomFunc (3 + dice 3) 2 " (Multiply (Const (VFloat 3.0)) (FnCall "randomFunc" [Plus (Const (VFloat 3.0)) (FnCall "dice" [Const (VFloat 3.0)]), Const (VFloat 2.0)])),
          testParseExpr "if randomFunc (3 + dice 3) 2 then 2 else 3" (IfThenElse (FnCall "randomFunc" [Plus (Const (VFloat 3.0)) (FnCall "dice" [Const (VFloat 3.0)]), Const (VFloat 2.0)]) (Const (VFloat 2.0)) (Const (VFloat 3.0)))
        ],
      testGroup
      "ProgramsWithOpt"
      [
          testParseProgram "main = 3 + Uniform" (Right [("main", Plus (Const (VFloat 3)) Uniform)]),
          testParseProgram "main = 3 + 7" (Right [("main", Const (VFloat 10))]),
          testParseProgram "main = 3 + 7; test = Uniform; test = 7" (Left "Function 'test' is already defined.")
      ]
    ]
