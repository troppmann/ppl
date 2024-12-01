module MaximumAPosteriori
  ( maxAPost,
  )
where

import Debug.Extended
import Evaluate (evalArithmetic)
import Interpret
import Query
import Representation

type ErrorString = String

-- TODO 19.11.24 maybe rename to mle
maxAPost :: Expr -> QueryType -> Either ErrorString (Dimension, Value)
maxAPost (Const c) QAny = return (0, c)
maxAPost Normal QAny = return (1, VFloat 0.0)
-- Any value between 0 and 1 is most likely, just picked the middle
maxAPost Uniform QAny = return (1, VFloat 0.5)
maxAPost (Plus e1 e2) QAny = mapArithmetic (+) e1 e2
maxAPost (Subtract e1 e2) QAny = mapArithmetic (-) e1 e2
maxAPost (Multiply e1 e2) QAny = mapArithmetic (*) e1 e2
maxAPost (Divide e1 e2) QAny = mapArithmetic (/) e1 e2
maxAPost (CreateTuple e1 e2) QAny = do
  (d1, v1) <- maxAPost e1 QAny
  (d2, v2) <- maxAPost e2 QAny
  return (d1 + d2, VTuple v1 v2)
maxAPost (CreateTuple e1 e2) (QTuple q1 q2) = do
  (d1, v1) <- maxAPost e1 q1
  (d2, v2) <- maxAPost e2 q2
  return (d1 + d2, VTuple v1 v2)
maxAPost (IfElseThen e1 e2 e3) query = do
  dimProbTrue@(dimTrue, probTrue) <- interpret e1 (VBool True)
  let dimProbFalse@(dimFalse, probFalse) = (0, 1.0) #-# dimProbTrue
  if dimTrue < dimFalse && probTrue > 0 || (0, 1.0) == dimProbTrue
    then
      maxAPost e2 query
    else
      if dimFalse < dimTrue && probFalse > 0 || (0, 1.0) == dimProbFalse
        then
          maxAPost e3 query
        else
          maxAPostIfElseThen (maxAPost e2 query) (maxAPost e3 query)
maxAPost e (QAt v) = do
  (dim, prob) <- interpret e (VFloat v)
  if prob /= 0.0
    then
      return (dim, VFloat v)
    else
      Left "Value is not possible."
maxAPost e q = todo $ "Missing case" <> show e <> show q

mapArithmetic :: (Double -> Double -> Double) -> Expr -> Expr -> Either ErrorString (Dimension, Value)
mapArithmetic op e1 e2 = do
  (d1, m1) <- maxAPost e1 QAny
  (d2, m2) <- maxAPost e2 QAny
  m3 <- evalArithmetic op m1 m2
  return (d1 + d2, m3)

maxAPostIfElseThen :: Either ErrorString (Dimension, Value) -> Either ErrorString (Dimension, Value) -> Either ErrorString (Dimension, Value)
maxAPostIfElseThen (Left e1) (Left e2) = Left $ show e1 <> " " <> show e2
maxAPostIfElseThen (Left "Value is not possible.") (Right (dim, value)) = return (dim, value)
maxAPostIfElseThen (Left e) (Right _) = Left e
maxAPostIfElseThen (Right (dim, value)) (Left "Value is not possible.") = return (dim, value)
maxAPostIfElseThen (Right _) (Left e) = Left e
maxAPostIfElseThen (Right (dim1, value1)) (Right (dim2, value2))
  | dim1 < dim2 = return (dim1, value1)
  | dim2 < dim1 = return (dim2, value2)
  | otherwise = Left "IfElseThen is not tractable if the query is not selective."