module MaximumAPosteriori
  ( MaximumAPosteriori.map,
  )
where

import Debug.Extended
import Evaluate (evalArithmetic)
import Interpret
import Query
import Representation
import Prelude hiding (map)

type ErrorString = String

-- TODO 19.11.24 maybe rename to mle
map :: Expr -> QueryType -> Either ErrorString (Dimension, Value)
map (Const c) QAny = return (0, c)
map Normal QAny = return (1, VFloat 0.0)
-- Any value between 0 and 1 is most likely, just picked the middle
map Uniform QAny = return (1, VFloat 0.5)
map (Plus e1 e2) QAny = mapArithmetic (+) e1 e2
map (Subtract e1 e2) QAny = mapArithmetic (-) e1 e2
map (Multiply e1 e2) QAny = mapArithmetic (*) e1 e2
map (Divide e1 e2) QAny = mapArithmetic (/) e1 e2
map (CreateTuple e1 e2) QAny = do
  (d1, v1) <- map e1 QAny
  (d2, v2) <- map e2 QAny
  return (d1 + d2, VTuple v1 v2)
map (CreateTuple e1 e2) (QTuple q1 q2) = do
  (d1, v1) <- map e1 q1
  (d2, v2) <- map e2 q2
  return (d1 + d2, VTuple v1 v2)
map (IfElseThen e1 e2 e3) query = do
  dimProbTrue@(dimTrue, probTrue) <- dbg $ interpret e1 (VBool True)
  let dimProbFalse@(dimFalse, probFalse) = (0, 1.0) #-# dimProbTrue
  (dimThen, probThen) <- dbg $ qInterpret e2 query
  (dimElse, probElse) <- dbg $ qInterpret e3 query
  -- let decide if one branch is always picked nonetheless
  if dimTrue < dimFalse && probTrue > 0 || (0, 1.0) == dimProbTrue
    then
      map e2 query
    else
      if dimFalse < dimTrue && probFalse > 0 || (0, 1.0) == dimProbFalse
        then
          map e3 query
        -- if both branches are possible decide on dimensionality of the branch
        else
          if dbg dimThen < dbg dimElse && dbg probThen > 0
            then map e2 query
            else
              if dimElse < dimThen && probElse > 0
                then
                  map e3 query
                -- if both are the same check if one side is not possible
                else
                  if probTrue /= 0 && probThen > 0 && probElse == 0
                    then
                      map e2 query
                    else
                      if probFalse /= 0 && probElse > 0 && probThen == 0
                        then
                          map e3 query
                        else
                          Left "IfElseThen is not tractable if the query is not selective."
map e (QAt v) = do
  dimProb@(dim, _) <- interpret e (VFloat v)
  if dimProb /= (0, 0.0)
    then
      return (dim, VFloat v)
    else
      Left "Value is not possible here."
map e q = todo $ "Missing case" <> show e <> show q

mapArithmetic :: (Double -> Double -> Double) -> Expr -> Expr -> Either ErrorString (Dimension, Value)
mapArithmetic op e1 e2 = do
  (d1, m1) <- map e1 QAny
  (d2, m2) <- map e2 QAny
  m3 <- evalArithmetic op m1 m2
  return (d1 + d2, m3)

-- could also be defined as an operator #<# in dimProb math
firstMoreLikelyThenSecond :: DimensionalProbability -> DimensionalProbability -> Bool
firstMoreLikelyThenSecond (dim1, prob1) (dim2, prob2)
  | dim1 < dim2 = True
  | dim1 > dim2 = False
  | otherwise = prob1 > prob2