module MaximumAPosteriori
  ( maxAPost,
    mle,
  )
where

import Debug.Extended
import Infer
import Query
import Evaluate
import Representation
import Optimizer
import Control.Monad
import Runtime

type ErrorString = String

mle :: Program -> QueryType -> Either ErrorString (DimensionalProbability, Value)
mle program query = do
  mainExpr <- justOr (lookup "main" program) "main-Func not found."
  let rt = Runtime {program, arguments = [], currentFnName = "main", recursionDepth = 0, maxRecursionDepth = 100}
  ((dim, prob), value) <- maxAPost rt mainExpr query
  (_dimCon, probCon) <- qInfer rt mainExpr Given query
  return ((dim, prob / probCon), value)


-- TODO 19.11.24 maybe rename to mle
maxAPost :: Runtime -> Expr -> QueryType -> Either ErrorString (DimensionalProbability, Value)
maxAPost _ _ QMar = return ((0, 1.0), VFloat (0/0))
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
-- this is only a shortcut to not write many QAny in a query
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
    then do 
      ((dim,prob), value) <- maxAPost rt e2 query
      return ((dim, probTrue * prob), value)
    else
      if dimFalse < dimTrue && probFalse > 0 || (0, 1.0) == dimProbFalse
        then do 
          ((dim,prob), value) <- maxAPost rt e3 query
          return ((dim, probFalse * prob), value)
        else
          maxAPostIfThenElse dimProbTrue <$> maxAPost rt e2 query <*> maxAPost rt e3 query
maxAPost rt expr (QFloat _ v) = do
  (dim, prob) <- infer rt expr (VFloat v)
  return ((dim, prob), VFloat v)
maxAPost rt (FnCall fnName arguments) query = do
  expr <- justOr (lookup fnName (program rt)) ("Fn '" ++ fnName ++ "' not found.")
  let newDepth = 1 + recursionDepth rt
  args <- traverse (optimizeExpr rt {recursionDepth= 0,maxRecursionDepth = 1} <=< replaceFnParameterWithContent rt) arguments
  let newRt = rt {recursionDepth = newDepth, arguments = args}
  if recursionDepth rt >= maxRecursionDepth rt
    then
      return ((0,0.0), VFloat (0/0))
    else
      maxAPost newRt expr query
maxAPost rt (FnParameter index) query
  | Just expr <- getElem (arguments rt) index = maxAPost rt expr query
  | otherwise = error $ "Could not find Parameter with index: " ++ show index
maxAPost rt expr@(LessThan _ _) QAny = maxAPostBooleanExpr rt expr
maxAPost rt expr@(LessThanOrEqual _ _) QAny = maxAPostBooleanExpr rt expr
maxAPost rt expr@(GreaterThan _ _) QAny = maxAPostBooleanExpr rt expr
maxAPost rt expr@(GreaterThanOrEqual _ _) QAny = maxAPostBooleanExpr rt expr
maxAPost rt expr@(Equal _ _) QAny = maxAPostBooleanExpr rt expr
maxAPost rt expr@(Unequal _ _) QAny = maxAPostBooleanExpr rt expr
maxAPost rt expr@(Or _ _) QAny = maxAPostBooleanExpr rt expr
maxAPost rt expr@(And _ _) QAny = maxAPostBooleanExpr rt expr
maxAPost rt expr@(Not _) QAny = maxAPostBooleanExpr rt expr
maxAPost _ e q = todo $ "Missing case" <> show e <> show q

maxAPostIfThenElse :: DimensionalProbability -> (DimensionalProbability, Value) -> (DimensionalProbability, Value) -> (DimensionalProbability, Value)
maxAPostIfThenElse (dimIf, probTrue) ((dim1, prob1), value1) ((dim2, prob2), value2)
  | dim1 < dim2 = ((dim1, probTrue * prob1), value1)
  | dim2 < dim1 = ((dim2, probFalse * prob2), value2)
  | dimIf == 0 && probTrue * prob1 > probFalse * prob2 = ((dim1, probTrue * prob1), value1)
  | otherwise = ((dim2, probFalse *prob2), value2)
  where
    probFalse = 1 - probTrue

maxAPostBooleanExpr :: Runtime -> Expr -> Either ErrorString (DimensionalProbability, Value) 
maxAPostBooleanExpr rt expr = do
  (dim, prob) <- infer rt expr (VBool True)
  if dim > 0 then
    return ((0, 1.0), VBool False)
  else if prob < 0.5 then
        return ((0, 1 - prob), VBool False)
      else 
        return ((0, prob), VBool True)