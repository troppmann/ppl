module MaximumAPosteriori
  ( maxAPost,
    mle,
  )
where

import Debug.Extended
import Evaluate (evalArithmetic)
import Infer
import Query
import Representation
import Runtime

type ErrorString = String

mle :: Program -> QueryType -> Either  ErrorString Value
mle program query = do
  mainExpr <- justOr (lookup "main" program) "main-Func not found."
  let runTime = Runtime {program, arguments = [], currentFnName="main", recursionDepth = 0, maxRecursionDepth = 100}
  dimValue <- maxAPost runTime mainExpr query
  return $ snd dimValue

-- TODO 19.11.24 maybe rename to mle
maxAPost :: Runtime -> Expr -> QueryType -> Either ErrorString (Dimension, Value)
maxAPost _ (Const c) QAny = return (0, c)
maxAPost _ Normal QAny = return (1, VFloat 0.0)
-- Any value between 0 and 1 is most likely, just picked the middle
maxAPost _ Uniform QAny = return (1, VFloat 0.5)
maxAPost rt (Plus e1 e2) QAny = mapArithmetic rt (+) e1 e2
maxAPost rt (Subtract e1 e2) QAny = mapArithmetic rt (-) e1 e2
maxAPost rt (Multiply e1 e2) QAny = mapArithmetic rt (*) e1 e2
maxAPost rt (Divide e1 e2) QAny = mapArithmetic rt (/) e1 e2
maxAPost rt (CreateTuple e1 e2) QAny = do
  (d1, v1) <- maxAPost rt e1 QAny
  (d2, v2) <- maxAPost rt e2 QAny
  return (d1 + d2, VTuple v1 v2)
maxAPost rt (CreateTuple e1 e2) (QTuple q1 q2) = do
  (d1, v1) <- maxAPost rt e1 q1
  (d2, v2) <- maxAPost rt e2 q2
  return (d1 + d2, VTuple v1 v2)
maxAPost rt (IfThenElse e1 e2 e3) query = do
  dimProbTrue@(dimTrue, probTrue) <- interpret rt e1 (VBool True)
  let dimProbFalse@(dimFalse, probFalse) = (0, 1.0) #-# dimProbTrue
  if dimTrue < dimFalse && probTrue > 0 || (0, 1.0) == dimProbTrue
    then
      maxAPost rt e2 query
    else
      if dimFalse < dimTrue && probFalse > 0 || (0, 1.0) == dimProbFalse
        then
          maxAPost rt e3 query
        else
          maxAPostIfThenElse dimProbTrue (maxAPost rt e2 query) (maxAPost rt e3 query)
maxAPost rt e (QAt v) = do
  (dim, prob) <- interpret rt e (VFloat v)
  if prob /= 0.0
    then
      return (dim, VFloat v)
    else
      Left "Value is not possible."
maxAPost _ e q = todo $ "Missing case" <> show e <> show q

mapArithmetic :: Runtime -> (Double -> Double -> Double) -> Expr -> Expr -> Either ErrorString (Dimension, Value)
mapArithmetic rt op e1 e2 = do
  (d1, m1) <- maxAPost rt e1 QAny
  (d2, m2) <- maxAPost rt e2 QAny
  m3 <- evalArithmetic op m1 m2
  return (d1 + d2, m3)

maxAPostIfThenElse :: DimensionalProbability -> Either ErrorString (Dimension, Value) -> Either ErrorString (Dimension, Value) -> Either ErrorString (Dimension, Value)
maxAPostIfThenElse _ (Left e1) (Left e2) = Left $ show e1 <> " " <> show e2
maxAPostIfThenElse _ (Left "Value is not possible.") (Right (dim, value)) = return (dim, value)
maxAPostIfThenElse _ (Left e) (Right _) = Left e
maxAPostIfThenElse _ (Right (dim, value)) (Left "Value is not possible.") = return (dim, value)
maxAPostIfThenElse _ (Right _) (Left e) = Left e
maxAPostIfThenElse (dimIf, probIf) (Right (dim1, value1)) (Right (dim2, value2))
  | dim1 < dim2 = return (dim1, value1)
  | dim2 < dim1 = return (dim2, value2)
  | dimIf == 0 && probIf > 0.5 = return (dim1, value1)
  | otherwise = return (dim2, value2)