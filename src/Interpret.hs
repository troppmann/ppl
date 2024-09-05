module Interpret
  ( interpret,
  )
where

import Debug.Extended
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
-- TODO 05.09.2024: Equal case can also be solved if one side is a constant,
--                  can maybe also be done in the inequality function
interpret (LessThan e1 e2) (VBool bool)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      x <- inequality e1 (ST, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      x <- inequality e2 (BT, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only interpret < with a one side Constant."
interpret (GreaterThan e1 e2) (VBool bool)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      x <- inequality e1 (BT, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      x <- inequality e2 (ST, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only interpret > with a one side Constant."
interpret (GreaterThan _ _) (VFloat _) = Right (0, 0.0)
interpret (LessThan _ _) (VFloat _) = Right (0, 0.0)
interpret (Equal e1 e2) (VFloat _) = Right (0, 0.0)
interpret e _ = todo ("Missing interpret case: " <> show e)

-- | ST = SmallerThan, BT = BiggerThan
-- TODO 04.09.2024: Change to Prelude.Ordering
data Inequality = ST | BT deriving (Show, Ord, Eq)

type InequalityQuery = (Inequality, Double)

swap :: Inequality -> Inequality
swap ST = BT
swap BT = ST

inequality :: Expr -> InequalityQuery -> Either String Double
inequality (Const (VFloat constant)) query
  | (ST, value) <- query = return $ if constant < value then 1.0 else 0.0
  | (BT, value) <- query = return $ if constant > value then 1.0 else 0.0
inequality Uniform query
  | (ST, value) <- query = return $ cumulative distr value
  | (BT, value) <- query = return $ complCumulative distr value
  where
    distr = uniformDistr 0.0 1.0
inequality Normal query
  | (ST, value) <- query = return $ cumulative distr value
  | (BT, value) <- query = return $ complCumulative distr value
  where
    distr = normalDistr 0.0 1.0
inequality (Plus e1 e2) query
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      inequality e2 (ie, value - c)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      inequality e1 (ie, value - c)
  where
    (ie, value) = query
inequality (Subtract e1 e2) query
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      inequality e2 (swap ie, -value + c)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      inequality e1 (ie, value + c)
  where
    (ie, value) = query
inequality (Multiply e1 e2) query
  -- TODO 04.09.24: handle c == 0.0
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      inequality e2 (if c < 0 then swap ie else ie, value / c)
  -- TODO 04.09.24: handle c == 0.0
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      inequality e1 (if c < 0 then swap ie else ie, value / c)
  where
    (ie, value) = query
inequality (Divide e1 e2) query
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      let bound = c / value
      if ie == ST
        then do
          -- Two Ranges either ([-∞, 0] and [bound, +∞]) or ([-∞, bound] and [0, +∞])
          firstRange <- inequality e2 (if bound < 0 then ST else BT, bound)
          secondRange <- inequality e2 (if bound < 0 then BT else ST, 0)
          return $ firstRange + secondRange
        else do
          -- One Range between Bound and 0 could either been [0, Bound] or [Bound,0]
          lower <- inequality e2 (ST, min bound 0)
          higher <- inequality e2 (ST, max bound 0)
          return $ higher - lower
  -- TODO 04.09.24: handle c == 0.0
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      inequality e1 (if c < 0 then swap ie else ie, value * c)
  where
    (ie, value) = query
inequality expr _ = todo $ "Missing integral case: " <> show expr