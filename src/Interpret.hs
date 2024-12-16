module Interpret
  ( interpret,
  )
where

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
              return (0, prob)
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
              return (0, prob)
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
interpret (Exponent e1 e2) value
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      if c == 0.0 then
        interpret (Const $ VFloat 1.0) value
      else do
        v <- evalAsFloat value
        let overC = 1 / c
        let sign = if v < 0 then (-1.0) else 1.0
        (dim, prob) <- interpret e1 (VFloat $ sign * (v ** overC))
        if dim == 0
          then
            return (0, prob)
          else
            return (1, prob * abs (overC * (v ** (overC - 1))))
  | otherwise = Left "Can only interpret Exponent(**) with a one side Constant."
interpret (IfThenElse e1 e2 e3) value = do
  dimProbTrue <- interpret e1 (VBool True)
  dimProbFalse <- interpret e1 (VBool False) -- (0, 1.0) #-# dimProbTrue
  dimProbBranchTrue <- interpret e2 value
  dimProbBranchFalse <- interpret e3 value
  return $ (dimProbTrue #*# dimProbBranchTrue) #+# (dimProbFalse #*# dimProbBranchFalse)
interpret (Equal e1 e2) (VBool bool)
  | Right constant <- evalConstExpr e2 = do
      dimProb <- interpret e1 constant
      return $ if bool then dimProb else (0, 1.0) #-# dimProb
  | Right constant <- evalConstExpr e1 = do
      dimProb <- interpret e2 constant
      return $ if bool then dimProb else (0, 1.0) #-# dimProb
  | otherwise = Left "Can only interpret == with a one side Constant."
interpret (Unequal e1 e2) (VBool bool) = interpret (Equal e1 e2) (VBool $ not bool)
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
  dimProb1True <- interpret e1 (VBool bool)
  let dimProb1False = (0, 1.0) #-# dimProb1True
  dimProb2True <- interpret e2 (VBool bool)
  let dimProb2False = (0, 1.0) #-# dimProb2True
  -- double negative vanishes dimensions over 0
  -- return $ (0, 1.0) #-# ((0, 1.0) #-# dimProb1True) #*# ((0, 1.0) #-# dimProb2True)
  return $ (dimProb1True #*# dimProb2True) #+# (dimProb1True #*# dimProb2False) #+# (dimProb1False #*# dimProb2True)
interpret (And e1 e2) (VBool bool) = do
  dimProb1 <- interpret e1 (VBool bool)
  dimProb2 <- interpret e2 (VBool bool)
  return $ dimProb1 #*# dimProb2
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
  return $ dimProbA #*# dimProbB
interpret _ (VTuple _ _) = Left "Can't interpret singular value expression with a tuple."
interpret (CreateTuple _ _) _ = Left "Can't interpret a tuple expression with a singular expression."

data CompareCase = LT | LE | GE | GT deriving (Ord, Enum, Show, Eq)

type CompareQuery = (CompareCase, Double)

swap :: CompareCase -> CompareCase
swap LT = GT
swap LE = GE
swap GT = LT
swap GE = LE

-- TODO 06.09.2024: Should i add a dimension differentiation?
compareFloatExpr :: Expr -> CompareQuery -> Either String Double
compareFloatExpr (Const (VBool _)) _ = Left "Expected Float got Bool."
compareFloatExpr (Const (VFloat constant)) (ord, value) = return $ case ord of
  LT -> if constant < value then 1.0 else 0.0
  LE -> if constant <= value then 1.0 else 0.0
  GT -> if constant > value then 1.0 else 0.0
  GE -> if constant >= value then 1.0 else 0.0
compareFloatExpr Uniform (ord, value) = return $ case ord of
  LT -> cumulative distr value
  LE -> cumulative distr value
  GT -> complCumulative distr value
  GE -> complCumulative distr value
  where
    distr = uniformDistr 0.0 1.0
compareFloatExpr Normal (ord, value) = return $ case ord of
  LT -> cumulative distr value
  LE -> cumulative distr value
  GT -> complCumulative distr value
  GE -> complCumulative distr value
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
          return $ if value == 0.0 then 1.0 else 0.0
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
            _ -> undefined
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      compareFloatExpr e1 (if c < 0 then swap ord else ord, value * c)
  | otherwise = Left "Can only interpret Divide(/) with a one side Constant."
compareFloatExpr (IfThenElse e1 e2 e3) (ord, value) = do
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