module MaximumAPosteriori
  ( maxAPost,
    mle,
  )
where

import Debug.Extended
import Evaluate (evalArithmetic, evalAsFloat, evalConstExpr)
import Infer
import Query
import Representation
import Runtime

type ErrorString = String

mle :: Program -> QueryType -> Either ErrorString (DimensionalProbability, Value)
mle program query = do
  mainExpr <- justOr (lookup "main" program) "main-Func not found."
  let rt = Runtime {program, arguments = [], currentFnName = "main", recursionDepth = 0, maxRecursionDepth = 100}
  maxAPost rt mainExpr query

-- TODO 19.11.24 maybe rename to mle
maxAPost :: Runtime -> Expr -> QueryType -> Either ErrorString (DimensionalProbability, Value)
maxAPost _ (Const c) QAny = return ((0, 1.0), c)
maxAPost _ Normal QAny = return ((1, 0.39894228040143265), VFloat 0.0)
-- Any value between 0 and 1 is most likely, just picked the middle
maxAPost _ Uniform QAny = return ((1, 1.0), VFloat 0.5)
maxAPost rt (Plus e1 e2) QAny
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      (dimProb, value) <- maxAPost rt e2 QAny
      v <- evalAsFloat value
      return (dimProb, VFloat (c + v))
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      (dimProb, value) <- maxAPost rt e1 QAny
      v <- evalAsFloat value
      return (dimProb, VFloat (v + c))
  | otherwise = Left "Can only mle Plus(+) with a one side Constant."
maxAPost rt (Subtract e1 e2) QAny
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      (dimProb, value) <- maxAPost rt e2 QAny
      v <- evalAsFloat value
      return (dimProb, VFloat (c - v))
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      (dimProb, value) <- maxAPost rt e1 QAny
      v <- evalAsFloat value
      return (dimProb, VFloat (v - c))
  | otherwise = Left "Can only mle Plus(+) with a one side Constant."
maxAPost rt (Multiply e1 e2) QAny
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      if c == 0.0
        then
          return ((0, 1.0), VFloat 0.0)
        else do
          ((dim, prob), value) <- maxAPost rt e2 QAny
          v <- evalAsFloat value
          case dim of
            0 -> return ((0, prob), VFloat (c * v))
            _ -> return ((1, prob / abs c), VFloat (c * v))
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      if c == 0.0
        then
          return ((0, 1.0), VFloat 0.0)
        else do
          ((dim, prob), value) <- maxAPost rt e1 QAny
          v <- evalAsFloat value
          case dim of
            0 -> return ((0, prob), VFloat (v * c))
            _ -> return ((1, prob / abs c), VFloat (v * c))
  | otherwise = Left "Can only infer Multiply(*) with a one side Constant."
maxAPost rt (Divide e1 e2) QAny
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      ((dim, prob), value) <- maxAPost rt e1 QAny
      v <- evalAsFloat value
      case dim of
        0 -> return ((0, prob), VFloat (v / c))
        _ -> return ((1, prob * abs c), VFloat (v / c))
  | otherwise = Left "Can only infer Divide(/) with a one side Constant."
maxAPost rt (CreateTuple e1 e2) QAny = do
  (dimProb1, v1) <- maxAPost rt e1 QAny
  (dimProb2, v2) <- maxAPost rt e2 QAny
  return (dimProb1 #*# dimProb2, VTuple v1 v2)
maxAPost rt (CreateTuple e1 e2) (QTuple q1 q2) = do
  (dimProb1, v1) <- maxAPost rt e1 q1
  (dimProb2, v2) <- maxAPost rt e2 q2
  return (dimProb1 #*# dimProb2, VTuple v1 v2)
maxAPost rt (IfThenElse e1 e2 e3) query = do
  dimProbTrue@(dimTrue, probTrue) <- infer rt e1 (VBool True)
  dimProbFalse@(dimFalse, probFalse) <- infer rt e1 (VBool False)
  if dimTrue < dimFalse && probTrue > 0 || (0, 1.0) == dimProbTrue
    then
      maxAPost rt e2 query
    else
      if dimFalse < dimTrue && probFalse > 0 || (0, 1.0) == dimProbFalse
        then
          maxAPost rt e3 query
        else
          maxAPostIfThenElse dimProbTrue (maxAPost rt e2 query) (maxAPost rt e3 query)
maxAPost rt expr (QAt v) = do
  (dim, prob) <- infer rt expr (VFloat v)
  if prob /= 0.0
    then
      return ((dim, prob), VFloat v)
    else
      Left "Value is not possible."
maxAPost _ e q = todo $ "Missing case" <> show e <> show q

maxAPostIfThenElse ::
  DimensionalProbability ->
  Either ErrorString (DimensionalProbability, Value) ->
  Either ErrorString (DimensionalProbability, Value) ->
  Either ErrorString (DimensionalProbability, Value)
maxAPostIfThenElse _ (Left e1) (Left e2) = Left $ show e1 <> " " <> show e2
maxAPostIfThenElse _ (Left "Value is not possible.") (Right (dim, value)) = return (dim, value)
maxAPostIfThenElse _ (Left e) (Right _) = Left e
maxAPostIfThenElse _ (Right (dim, value)) (Left "Value is not possible.") = return (dim, value)
maxAPostIfThenElse _ (Right _) (Left e) = Left e
maxAPostIfThenElse (dimIf, probIf) (Right ((dim1,prob1), value1)) (Right ((dim2,prob2), value2))
  | dim1 < dim2 = return ((dim1,prob1), value1)
  | dim2 < dim1 = return ((dim2,prob2), value2)
  | dimIf == 0 && probIf * prob1 > (1-probIf) * prob2 = return ((dim1,prob1), value1)
  | otherwise = return ((dim2,prob2), value2)