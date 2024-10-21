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
import Prelude hiding (EQ, GT, LT)

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
  | otherwise = Left "Can only interpret Subtract(-) with a one side Constant."
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
  | otherwise = Left "Can only interpret Multiply(*) with a one side Constant."
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
  dimProbTrue@(dim, probTrue) <- interpret e1 (VBool True)
  let dimProbFalse = (dim, 1.0 - probTrue)
  dimProbBranchTrue <- interpret e2 value
  dimProbBranchFalse <- interpret e3 value
  return $ (dimProbTrue ⊙ dimProbBranchTrue) ⊕ (dimProbFalse ⊙ dimProbBranchFalse)
interpret (Equal e1 e2) (VBool bool)
  | Right constant <- evalConstExpr e2 = case constant of
      (VFloat c) -> do
        prob <- compareFloatExpr e1 (EQ, c)
        return (0, if bool then prob else 1 - prob)
      c -> do
        (dim, prob) <- interpret e1 c
        return (dim, if bool then prob else 1 - prob)
  | Right constant <- evalConstExpr e1 = case constant of
      (VFloat c) -> do
        prob <- compareFloatExpr e2 (EQ, c)
        return (0, if bool then prob else 1 - prob)
      c -> do
        (dim, prob) <- interpret e2 c
        return (dim, if bool then prob else 1 - prob)
  | otherwise = Left "Can only interpret == with a one side Constant."
interpret (Unequal e1 e2) (VBool bool) = do
  (dim, prob) <- interpret (Equal e1 e2) (VBool bool)
  return (dim, 1 - prob)
interpret (LessThanOrEqual e1 e2) (VBool bool)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr e1 (LE, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr e2 (GE, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only interpret < with a one side Constant."
interpret (LessThan e1 e2) (VBool bool)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr e1 (LT, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr e2 (GT, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only interpret < with a one side Constant."
interpret (GreaterThan e1 e2) (VBool bool)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr e1 (GT, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr e2 (LT, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only interpret > with a one side Constant."
interpret (GreaterThanOrEqual e1 e2) (VBool bool)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr e1 (GE, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr e2 (LE, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only interpret > with a one side Constant."
interpret (Or e1 e2) (VBool bool) = do
  (_dim, p1) <- interpret e1 (VBool bool)
  (_dim, p2) <- interpret e2 (VBool bool)
  return (0, 1 - (1 - p1) * (1 - p2))
interpret (And e1 e2) (VBool bool) = do
  (_dim, p1) <- interpret e1 (VBool bool)
  (_dim, p2) <- interpret e2 (VBool bool)
  return (0, p1 * p2)
interpret (Not e1) (VBool bool) = interpret e1 (VBool $ not bool)
-- TODO 06.09.2024 Should this throw an error instead?
interpret Uniform (VBool _) = Right (0, 0.0)
interpret Normal (VBool _) = Right (0, 0.0)
interpret (GreaterThan _ _) (VFloat _) = Right (0, 0.0)
interpret (GreaterThanOrEqual _ _) (VFloat _) = Right (0, 0.0)
interpret (LessThan _ _) (VFloat _) = Right (0, 0.0)
interpret (LessThanOrEqual _ _) (VFloat _) = Right (0, 0.0)
interpret (Equal _ _) (VFloat _) = Right (0, 0.0)
interpret (Unequal _ _) (VFloat _) = Right (0, 0.0)
interpret (And _ _) (VFloat _) = Right (0, 0.0)
interpret (Or _ _) (VFloat _) = Right (0, 0.0)
interpret (Not _) (VFloat _) = Right (0, 0.0)
interpret (CreateTuple e1 e2) (VTuple v1 v2) = do
  dimProbA <- interpret e1 v1
  dimProbB <- interpret e2 v2
  return $ dimProbA ⊙ dimProbB
interpret e _ = todo ("Missing interpret case: " <> show e)

data CompareCase = LT | LE | EQ | GE | GT deriving (Ord, Enum, Show, Eq)

type CompareQuery = (CompareCase, Double)

swap :: CompareCase -> CompareCase
swap LT = GT
swap LE = GE
swap GT = LT
swap GE = LE
swap EQ = EQ

-- | Epsilon with IEEE754 doubles \n
-- The smallest positive value x such that 1 + x is representable.
epsilon :: Double
epsilon = 2.2204460492503131e-16

inRange :: (Ord a) => (a, a) -> a -> Bool
inRange (minA, maxB) value = minA <= value && value <= maxB

-- TODO 06.09.2024: Should i add a dimension differentiation?
compareFloatExpr :: Expr -> CompareQuery -> Either String Double
compareFloatExpr (Const (VBool _)) _ = Left "Expected Float got Bool."
compareFloatExpr (Const (VFloat constant)) (ord, value) = return $ case ord of
  LT -> if constant < value then 1.0 else 0.0
  LE -> if constant <= value then 1.0 else 0.0
  GT -> if constant > value then 1.0 else 0.0
  GE -> if constant >= value then 1.0 else 0.0
  EQ -> if constant == value then 1.0 else 0.0
compareFloatExpr Uniform (ord, value) = return $ case ord of
  LT -> cumulative distr value
  LE -> cumulative distr value
  GT -> complCumulative distr value
  GE -> complCumulative distr value
  EQ -> if inRange (0.0, 1.0) value then epsilon else 0.0
  where
    distr = uniformDistr 0.0 1.0
compareFloatExpr Normal (ord, value) = return $ case ord of
  LT -> cumulative distr value
  LE -> cumulative distr value
  GT -> complCumulative distr value
  GE -> complCumulative distr value
  EQ -> epsilon
  where
    distr = normalDistr 0.0 1.0
compareFloatExpr (Plus e1 e2) (ord, value)
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      compareFloatExpr e2 (ord, value - c)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      compareFloatExpr e1 (ord, value - c)
  | otherwise = Left "Can only interpret Multiply(*) with a one side Constant."
compareFloatExpr (Subtract e1 e2) (ord, value)
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      compareFloatExpr e2 (swap ord, -value + c)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      compareFloatExpr e1 (ord, value + c)
  | otherwise = Left "Can only interpret Subtract(-) with a one side Constant."
compareFloatExpr (Multiply e1 e2) (ord, value)
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      if c == 0
        then
          compareFloatExpr (Const (VFloat 0.0)) (ord, value)
        else
          compareFloatExpr e2 (if c < 0 then swap ord else ord, value / c)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      if c == 0
        then
          compareFloatExpr (Const (VFloat 0.0)) (ord, value)
        else
          compareFloatExpr e1 (if c < 0 then swap ord else ord, value / c)
  | otherwise = Left "Can only interpret Multiply(*) with a one side Constant."
compareFloatExpr (Divide e1 e2) (ord, value)
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      if c == 0.0
        then
          compareFloatExpr (Const $ VFloat 0.0) (EQ, value)
        else do
          let bound = c / value
          case ord of
            -- Two Ranges either ([-∞, 0] and [bound, +∞]) or ([-∞, bound] and [0, +∞])
            lt | lt `elem` [LT, LE] -> do
              firstRange <- compareFloatExpr e2 (if bound < 0 then ord else swap ord, bound)
              secondRange <- compareFloatExpr e2 (if bound < 0 then swap ord else ord, 0)
              return $ firstRange + secondRange
            -- One Range between Bound and 0 could either been [0, Bound] or [Bound,0]
            gt | gt `elem` [GT, GE] -> do
              lower <- compareFloatExpr e2 (swap ord, min bound 0)
              higher <- compareFloatExpr e2 (swap ord, max bound 0)
              return $ higher - lower
            -- c / e2 == value => e2 == c / value == bound
            _eq -> compareFloatExpr e2 (EQ, bound)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      compareFloatExpr e1 (if c < 0 then swap ord else ord, value * c)
  | otherwise = Left "Can only interpret Divide(/) with a one side Constant."
compareFloatExpr (IfElseThen e1 e2 e3) (ord, value) = do
  (_dim, probTrue) <- interpret e1 (VBool True)
  let probFalse = 1 - probTrue
  p2 <- compareFloatExpr e2 (ord, value)
  p3 <- compareFloatExpr e3 (ord, value)
  return $ probTrue * p2 + probFalse * p3
compareFloatExpr expr _ = case expr of
  (CreateTuple _ _) -> msgTuple
  (Const (VTuple _ _)) -> msgTuple
  (And _ _) -> msgBool
  (Or _ _) -> msgBool
  (Not _) -> msgBool
  (Equal _ _) -> msgBool
  (Unequal _ _) -> msgBool
  (LessThan _ _) -> msgBool
  (LessThanOrEqual _ _) -> msgBool
  (GreaterThan _ _) -> msgBool
  (GreaterThanOrEqual _ _) -> msgBool
  where
    msg = "Expected Float got " <> show expr <> " that evaluates to a"
    msgBool = Left $ msg <> "Bool."
    msgTuple = Left $ msg <> "Tuple."