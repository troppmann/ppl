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
interpret (Equal e1 e2) (VBool bool)
  -- TODO 05.09.2024: VBool VBool can also be compared
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      x <- compareExpr e1 (EQ, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      x <- compareExpr e2 (EQ, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only interpret == with a one side Constant."
interpret (Unequal e1 e2) (VBool bool) = do
  (dim, prob) <- interpret (Equal e1 e2) (VBool bool)
  return (dim, 1 - prob)
interpret (LessThan e1 e2) (VBool bool)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      x <- compareExpr e1 (LT, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      x <- compareExpr e2 (GT, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only interpret < with a one side Constant."
interpret (GreaterThan e1 e2) (VBool bool)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      x <- compareExpr e1 (GT, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      x <- compareExpr e2 (LT, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only interpret > with a one side Constant."
interpret (GreaterThan _ _) (VFloat _) = Right (0, 0.0)
interpret (LessThan _ _) (VFloat _) = Right (0, 0.0)
interpret (Equal _ _) (VFloat _) = Right (0, 0.0)
interpret (Unequal _ _) (VFloat _) = Right (0, 0.0)
interpret e _ = todo ("Missing interpret case: " <> show e)

type CompareQuery = (Ordering, Double)

swap :: Ordering -> Ordering
swap LT = GT
swap GT = LT
swap EQ = EQ

-- | Epsilon with IEEE754 doubles \n
-- The smallest positive value x such that 1 + x is representable.
epsilon :: Double
epsilon = 2.2204460492503131e-16

inRange :: (Ord a) => (a, a) -> a -> Bool
inRange (minA, maxB) value = minA <= value && value <= maxB

compareExpr :: Expr -> CompareQuery -> Either String Double
compareExpr (Const (VFloat constant)) query
  | (LT, value) <- query = return $ if constant < value then 1.0 else 0.0
  | (GT, value) <- query = return $ if constant > value then 1.0 else 0.0
  | (EQ, value) <- query = return $ if constant == value then 1.0 else 0.0
compareExpr Uniform query
  | (LT, value) <- query = return $ cumulative distr value
  | (GT, value) <- query = return $ complCumulative distr value
  | (EQ, value) <- query = return $ if inRange (0.0, 1.0) value then epsilon else 0.0
  where
    distr = uniformDistr 0.0 1.0
compareExpr Normal query
  | (LT, value) <- query = return $ cumulative distr value
  | (GT, value) <- query = return $ complCumulative distr value
  | (EQ, _value) <- query = return epsilon
  where
    distr = normalDistr 0.0 1.0
compareExpr (Plus e1 e2) query
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      compareExpr e2 (ord, value - c)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      compareExpr e1 (ord, value - c)
  where
    (ord, value) = query
compareExpr (Subtract e1 e2) query
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      compareExpr e2 (swap ord, -value + c)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      compareExpr e1 (ord, value + c)
  where
    (ord, value) = query
compareExpr (Multiply e1 e2) query
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      if c == 0
        then
          compareExpr (Const (VFloat 0.0)) (ord, value)
        else
          compareExpr e2 (if c < 0 then swap ord else ord, value / c)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      if c == 0
        then
          compareExpr (Const (VFloat 0.0)) (ord, value)
        else
          compareExpr e1 (if c < 0 then swap ord else ord, value / c)
  where
    (ord, value) = query
compareExpr (Divide e1 e2) query
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      if c == 0.0
        then
          compareExpr (Const $ VFloat 0.0) (EQ, value)
        else do
          let bound = c / value
          case ord of
            -- Two Ranges either ([-∞, 0] and [bound, +∞]) or ([-∞, bound] and [0, +∞])
            LT -> do
              firstRange <- compareExpr e2 (if bound < 0 then LT else GT, bound)
              secondRange <- compareExpr e2 (if bound < 0 then GT else LT, 0)
              return $ firstRange + secondRange
            -- One Range between Bound and 0 could either been [0, Bound] or [Bound,0]
            GT -> do
              lower <- compareExpr e2 (LT, min bound 0)
              higher <- compareExpr e2 (LT, max bound 0)
              return $ higher - lower
            -- c / e2 == value => e2 == c / value == bound
            EQ -> compareExpr e2 (EQ, bound)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      compareExpr e1 (if c < 0 then swap ord else ord, value * c)
  where
    (ord, value) = query
compareExpr expr _ = todo $ "Missing compareExpr case: " <> show expr