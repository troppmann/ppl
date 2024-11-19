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
map :: Expr -> QueryType -> Either ErrorString Value
map (Const c) QAny = return c
map Normal QAny = return $ VFloat 0.0
-- Any value between 0 and 1 is most likely, just picked the middle
map Uniform QAny = return $ VFloat 0.5
map (Plus e1 e2) QAny = do
  m1 <- map e1 QAny
  m2 <- map e2 QAny
  evalArithmetic (+) m1 m2
map (Subtract e1 e2) QAny = do
  m1 <- map e1 QAny
  m2 <- map e2 QAny
  evalArithmetic (-) m1 m2
map (Multiply e1 e2) QAny = do
  m1 <- map e1 QAny
  m2 <- map e2 QAny
  evalArithmetic (*) m1 m2
map (Divide e1 e2) QAny = do
  m1 <- map e1 QAny
  m2 <- map e2 QAny
  evalArithmetic (/) m1 m2
map (IfElseThen e1 e2 e3) QAny = do
  (dim, prob) <- interpret e1 (VBool True)
  if (dim == 0) && (prob > 0.5)
    then
      map e2 QAny
    else
      map e3 QAny
map (CreateTuple e1 e2) QAny = do
  v1 <- map e1 QAny
  v2 <- map e2 QAny
  return $ VTuple v1 v2
map e q = todo $ "Missing case" <> show e <> show q
