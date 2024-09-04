module Interpret
  ( interpret,
    integral,
  )
where

import Evaluate
import Representation
import Statistics.Distribution
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Uniform

interpret :: Expr -> Value -> Either String DimensionalProbability
interpret Uniform (VFloat f) = Right (1, density distr f)
  where
    distr = uniformDistr 0.0 1.0
interpret Normal (VFloat f) = Right (1, density distr f)
  where
    distr = normalDistr 0.0 1.0
interpret (Const v1) value = Right (0, if v1 == value then 1.0 else 0.0)
interpret (Plus e1 e2) value
  | Right constant <- evalConstExpr e1 = do
      newValue <- evalArithmetic (-) value constant
      interpret e2 newValue
  | Right constant <- evalConstExpr e2 = do
      newValue <- evalArithmetic (-) value constant
      interpret e1 newValue
  | otherwise = Left "Can only interpret Plus(+) with a one side Constant."
interpret (Subtract e1 e2) value
  | Right constant <- evalConstExpr e1 = do
      newValue <- evalArithmetic (-) constant value
      interpret e2 newValue
  | Right constant <- evalConstExpr e2 = do
      newValue <- evalArithmetic (+) value constant
      interpret e1 newValue
  | otherwise = Left "Can only interpret Subtract(+) with a one side Constant."
interpret (Multiply e1 e2) value
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      if c == 0.0
        then
          interpret (Const $ VFloat 0.0) value
        else do
          v <- evalAsFloat value
          (dim, prob) <- interpret e2 (VFloat $ v / c)
          if dim == 0
            then
              return (dim, prob)
            else
              return (1, prob / abs c)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      if c == 0.0
        then
          interpret (Const $ VFloat 0.0) value
        else do
          v <- evalAsFloat value
          (dim, prob) <- interpret e1 (VFloat $ v / c)
          if dim == 0
            then
              return (dim, prob)
            else
              return (1, prob / abs c)
  | otherwise = Left "Can only interpret Multiply(+) with a one side Constant."
interpret (Divide e1 e2) value
  | Right constant <- evalConstExpr e1 = do
      -- TODO works only on monotone functions and c != 0.0 and v != 0.0
      c <- evalAsFloat constant
      v <- evalAsFloat value
      (_dim, prob) <- interpret e2 (VFloat $ c / v)
      return (1, prob * abs ((-c) / (v * v)))
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      v <- evalAsFloat value
      (_dim, prob) <- interpret e1 (VFloat $ v * c)
      return (1, prob * abs c)
  | otherwise = Left "Can only interpret Divide(/) with a one side Constant."
interpret (IfElseThen e1 e2 e3) value = do
  -- TODO handle dimension
  (_dim1, probTrue) <- interpret e1 (VBool True)
  let probFalse = 1 - probTrue
  (_dim2, p2) <- interpret e2 value
  (_dim3, p3) <- interpret e3 value
  return (1, probTrue * p2 + probFalse * p3)
-- TODO 03.09.2024: Add LessThanEqual and GreaterThanEqual
interpret (LessThan e1 e2) (VBool bool)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      x <- integral e1 (SmallerThan c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      x <- integral e2 (BiggerThan c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only interpret < with a one side Constant."
interpret (GreaterThan e1 e2) (VBool bool)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      x <- integral e1 (BiggerThan c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      x <- integral e2 (SmallerThan c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only interpret > with a one side Constant."
interpret (GreaterThan _ _) (VFloat f) = Left $ "GreaterThan (>) can't be evaluate to a Float " <> show f
interpret (LessThan _ _) (VFloat f) = Left $ "GreaterThan (>) can't be evaluate to a Float " <> show f
interpret e _ = todo ("Missing interpret case: " <> show e)

todo :: String -> a
todo msg = error ("not yet implemented: " ++ msg)

data IntegralQuery = SmallerThan Double | BiggerThan Double

integral :: Expr -> IntegralQuery -> Either String Double
integral Uniform (SmallerThan f) = Right $ cumulative distr f
  where
    distr = uniformDistr 0.0 1.0
integral Uniform (BiggerThan f) = Right $ complCumulative distr f
  where
    distr = uniformDistr 0.0 1.0
integral Normal (SmallerThan f) = Right $ cumulative distr f
  where
    distr = normalDistr 0.0 1.0
integral Normal (BiggerThan f) = Right $ complCumulative distr f
  where
    distr = normalDistr 0.0 1.0
integral expr _ = todo $ "Missing integral case: " <> show expr