module Interpret
  ( inferProgram,
    interpret,
  )
where

import Debug.Extended
import Evaluate
import Representation
import Runtime
import Statistics.Distribution
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Uniform
import Prelude hiding (EQ, GT, LT)

type ErrorString = String

inferProgram :: Program -> Value -> Either ErrorString DimensionalProbability
inferProgram program value = do
  mainExpr <- justOr (lookup "main" program) "main-Func not found."
  let runTime = InferRuntime {program, arguments = [], currentFnName="main", recursionDepth = 0, maxRecursionDepth = 10}
  interpret runTime mainExpr value

interpret :: InferRuntime -> Expr -> Value -> Either String DimensionalProbability
interpret _ Uniform (VFloat v) = Right (1, if 0.0 <= v && v < 1.0 then 1.0 else 0.0)
interpret _ Normal (VFloat v) = Right (1, density distr v)
  where
    distr = normalDistr 0.0 1.0
interpret _ (Const v1) value = Right (0, if v1 == value then 1.0 else 0.0)
interpret rt (Plus e1 e2) value
  | Right constant <- evalConstExpr rt e1 = do
      newValue <- evalArithmetic (-) value constant
      interpret rt e2 newValue
  | Right constant <- evalConstExpr rt e2 = do
      newValue <- evalArithmetic (-) value constant
      interpret rt e1 newValue
  | otherwise = Left "Can only interpret Plus(+) with a one side Constant."
interpret rt (Subtract e1 e2) value
  | Right constant <- evalConstExpr rt e1 = do
      newValue <- evalArithmetic (-) constant value
      interpret rt e2 newValue
  | Right constant <- evalConstExpr rt e2 = do
      newValue <- evalArithmetic (+) value constant
      interpret rt e1 newValue
  | otherwise = Left "Can only interpret Subtract(-) with a one side Constant."
interpret rt (Multiply e1 e2) value
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      if c == 0.0
        then
          interpret rt (Const $ VFloat 0.0) value
        else do
          v <- evalAsFloat value
          (dim, prob) <- interpret rt e2 (VFloat $ v / c)
          if dim == 0
            then
              return (0, prob)
            else
              return (1, prob / abs c)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      if c == 0.0
        then
          interpret rt (Const $ VFloat 0.0) value
        else do
          v <- evalAsFloat value
          (dim, prob) <- interpret rt e1 (VFloat $ v / c)
          if dim == 0
            then
              return (0, prob)
            else
              return (1, prob / abs c)
  | otherwise = Left "Can only interpret Multiply(*) with a one side Constant."
interpret rt (Divide e1 e2) value
  | Right constant <- evalConstExpr rt e1 = do
      -- TODO works only on monotone functions and c != 0.0 and v != 0.0
      c <- evalAsFloat constant
      v <- evalAsFloat value
      (_dim, prob) <- interpret rt e2 (VFloat $ c / v)
      return (1, prob * abs ((-c) / (v * v)))
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      v <- evalAsFloat value
      (_dim, prob) <- interpret rt e1 (VFloat $ v * c)
      return (1, prob * abs c)
  | otherwise = Left "Can only interpret Divide(/) with a one side Constant."
interpret rt (Exponent e1 e2) value
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      if c == 0.0
        then
          interpret rt (Const $ VFloat 1.0) value
        else do
          v <- evalAsFloat value
          let overC = 1 / c
          let sign = if v < 0 then (-1.0) else 1.0
          (dim, prob) <- interpret rt e1 (VFloat $ sign * (v ** overC))
          if dim == 0
            then
              return (0, prob)
            else
              return (1, prob * abs (overC * (v ** (overC - 1))))
  | otherwise = Left "Can only interpret Exponent(**) with a one side Constant."
interpret rt (IfThenElse e1 e2 e3) value = do
  dimProbTrue <- interpret rt e1 (VBool True)
  dimProbFalse <- interpret rt e1 (VBool False) -- (0, 1.0) #-# dimProbTrue
  dimProbBranchTrue <- interpret rt e2 value
  dimProbBranchFalse <- interpret rt e3 value
  return $ (dimProbTrue #*# dimProbBranchTrue) #+# (dimProbFalse #*# dimProbBranchFalse)
interpret rt (Equal e1 e2) (VBool bool)
  | Right constant <- evalConstExpr rt e2 = do
      dimProb <- interpret rt e1 constant
      return $ if bool then dimProb else (0, 1.0) #-# dimProb
  | Right constant <- evalConstExpr rt e1 = do
      dimProb <- interpret rt e2 constant
      return $ if bool then dimProb else (0, 1.0) #-# dimProb
  | otherwise = Left "Can only interpret == with a one side Constant."
interpret rt (Unequal e1 e2) (VBool bool) = interpret rt (Equal e1 e2) (VBool $ not bool)
interpret rt (LessThanOrEqual e1 e2) (VBool bool)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e1 (LE, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e2 (GE, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only interpret <= with a one side Constant."
interpret rt (LessThan e1 e2) (VBool bool)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e1 (LT, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e2 (GT, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only interpret <= with a one side Constant."
interpret rt (GreaterThan e1 e2) (VBool bool)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e1 (GT, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e2 (LT, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only interpret > with a one side Constant."
interpret rt (GreaterThanOrEqual e1 e2) (VBool bool)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e1 (GE, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e2 (LE, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only interpret > with a one side Constant."
interpret rt (Or e1 e2) (VBool bool) = do
  dimProb1True <- interpret rt e1 (VBool bool)
  let dimProb1False = (0, 1.0) #-# dimProb1True
  dimProb2True <- interpret rt e2 (VBool bool)
  let dimProb2False = (0, 1.0) #-# dimProb2True
  -- double negative vanishes dimensions over 0
  -- return $ (0, 1.0) #-# ((0, 1.0) #-# dimProb1True) #*# ((0, 1.0) #-# dimProb2True)
  return $ (dimProb1True #*# dimProb2True) #+# (dimProb1True #*# dimProb2False) #+# (dimProb1False #*# dimProb2True)
interpret rt (And e1 e2) (VBool bool) = do
  dimProb1 <- interpret rt e1 (VBool bool)
  dimProb2 <- interpret rt e2 (VBool bool)
  return $ dimProb1 #*# dimProb2
interpret rt (Not e1) (VBool bool) = interpret rt e1 (VBool $ not bool)
-- TODO 06.09.2024 Should this throw an error instead?
interpret _ Uniform (VBool _) = Right (0, 0.0)
interpret _ Normal (VBool _) = Right (0, 0.0)
interpret _ (GreaterThan _ _) (VFloat _) = Right (0, 0.0)
interpret _ (GreaterThanOrEqual _ _) (VFloat _) = Right (0, 0.0)
interpret _ (LessThan _ _) (VFloat _) = Right (0, 0.0)
interpret _ (LessThanOrEqual _ _) (VFloat _) = Right (0, 0.0)
interpret _ (Equal _ _) (VFloat _) = Right (0, 0.0)
interpret _ (Unequal _ _) (VFloat _) = Right (0, 0.0)
interpret _ (And _ _) (VFloat _) = Right (0, 0.0)
interpret _ (Or _ _) (VFloat _) = Right (0, 0.0)
interpret _ (Not _) (VFloat _) = Right (0, 0.0)
interpret rt (CreateTuple e1 e2) (VTuple v1 v2) = do
  dimProbA <- interpret rt e1 v1
  dimProbB <- interpret rt e2 v2
  return $ dimProbA #*# dimProbB
interpret _ _ (VTuple _ _) = Left "Can't interpret singular value expression with a tuple."
interpret _ (CreateTuple _ _) _ = Left "Can't interpret a tuple expression with a singular expression."
interpret rt (FnCall fnName arguments) val = do
  expr <- justOr (lookup fnName (program rt)) ("Fn '" ++ fnName ++ "' not found.")
  let newDepth = 1 + recursionDepth rt
  args <- traverse (replaceFnParameterWithContent rt) arguments
  let newRt = rt {recursionDepth = newDepth, arguments=args}
  if newDepth >= maxRecursionDepth rt then
    Right (0, 0.0)
  else
    interpret newRt expr val
interpret rt (FnParameter index) val
  | Just ele <- getElem (arguments rt) index = interpret rt ele val
  | otherwise = error $ "Could not find Parameter with index: " ++ show index


replaceFnParameterWithContent :: InferRuntime -> Expr -> Either ErrorString Expr
replaceFnParameterWithContent rt (FnParameter index)
  | Just ele <- getElem (arguments rt) index = return ele
  | otherwise = error $ "Could not find Parameter with index: " ++ show index
replaceFnParameterWithContent rt (Plus e1 e2) = checkBothBranches rt Plus e1 e2
replaceFnParameterWithContent rt (Subtract e1 e2) = checkBothBranches rt Subtract e1 e2
replaceFnParameterWithContent rt (Multiply e1 e2) = checkBothBranches rt Multiply e1 e2
replaceFnParameterWithContent rt (Divide e1 e2) = checkBothBranches rt Divide e1 e2
replaceFnParameterWithContent rt (And e1 e2) = checkBothBranches rt And e1 e2
replaceFnParameterWithContent rt (Or e1 e2) = checkBothBranches rt Or e1 e2
replaceFnParameterWithContent rt (LessThan e1 e2) = checkBothBranches rt LessThan e1 e2
replaceFnParameterWithContent rt (LessThanOrEqual e1 e2) = checkBothBranches rt LessThanOrEqual e1 e2
replaceFnParameterWithContent rt (GreaterThan e1 e2) = checkBothBranches rt GreaterThan e1 e2
replaceFnParameterWithContent rt (GreaterThanOrEqual e1 e2) = checkBothBranches rt GreaterThanOrEqual e1 e2
replaceFnParameterWithContent rt (Equal e1 e2) = checkBothBranches rt Equal e1 e2
replaceFnParameterWithContent rt (Unequal e1 e2) = checkBothBranches rt Unequal e1 e2
replaceFnParameterWithContent rt (Not e1) = do
  r1 <- replaceFnParameterWithContent rt e1
  return $ Not r1
replaceFnParameterWithContent _ expr = return expr

checkBothBranches :: InferRuntime -> (Expr -> Expr -> Expr) -> Expr -> Expr -> Either ErrorString Expr
checkBothBranches rt exprF e1 e2 = do
  r1 <- replaceFnParameterWithContent rt e1
  r2 <- replaceFnParameterWithContent rt e2
  return $ exprF r1 r2
data CompareCase = LT | LE | GE | GT deriving (Ord, Enum, Show, Eq)

type CompareQuery = (CompareCase, Double)

swap :: CompareCase -> CompareCase
swap LT = GT
swap LE = GE
swap GT = LT
swap GE = LE

-- TODO 06.09.2024: Should i add a dimension differentiation?
compareFloatExpr :: InferRuntime -> Expr -> CompareQuery -> Either String Double
compareFloatExpr _ (Const (VBool _)) _ = Left "Expected Float got Bool."
compareFloatExpr _ (Const (VFloat constant)) (ord, value) = return $ case ord of
  LT -> if constant < value then 1.0 else 0.0
  LE -> if constant <= value then 1.0 else 0.0
  GT -> if constant > value then 1.0 else 0.0
  GE -> if constant >= value then 1.0 else 0.0
compareFloatExpr _ Uniform (ord, value) = return $ case ord of
  LT -> cumulative distr value
  LE -> cumulative distr value
  GT -> complCumulative distr value
  GE -> complCumulative distr value
  where
    distr = uniformDistr 0.0 1.0
compareFloatExpr _ Normal (ord, value) = return $ case ord of
  LT -> cumulative distr value
  LE -> cumulative distr value
  GT -> complCumulative distr value
  GE -> complCumulative distr value
  where
    distr = normalDistr 0.0 1.0
compareFloatExpr rt (Plus e1 e2) (ord, value)
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      compareFloatExpr rt e2 (ord, value - c)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      compareFloatExpr rt e1 (ord, value - c)
  | otherwise = Left "Can only interpret Multiply(*) with a one side Constant."
compareFloatExpr rt (Subtract e1 e2) (ord, value)
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      compareFloatExpr rt e2 (swap ord, -value + c)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      compareFloatExpr rt e1 (ord, value + c)
  | otherwise = Left "Can only interpret Subtract(-) with a one side Constant."
compareFloatExpr rt (Multiply e1 e2) (ord, value)
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      if c == 0
        then
          compareFloatExpr rt (Const (VFloat 0.0)) (ord, value)
        else
          compareFloatExpr rt e2 (if c < 0 then swap ord else ord, value / c)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      if c == 0
        then
          compareFloatExpr rt (Const (VFloat 0.0)) (ord, value)
        else
          compareFloatExpr rt e1 (if c < 0 then swap ord else ord, value / c)
  | otherwise = Left "Can only interpret Multiply(*) with a one side Constant."
compareFloatExpr rt (Divide e1 e2) (ord, value)
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      if c == 0.0
        then
          return $ if value == 0.0 then 1.0 else 0.0
        else do
          let bound = c / value
          case ord of
            -- Two Ranges either ([-∞, 0] and [bound, +∞]) or ([-∞, bound] and [0, +∞])
            lt | lt `elem` [LT, LE] -> do
              firstRange <- compareFloatExpr rt e2 (if bound < 0 then ord else swap ord, bound)
              secondRange <- compareFloatExpr rt e2 (if bound < 0 then swap ord else ord, 0)
              return $ firstRange + secondRange
            -- One Range between Bound and 0 could either been [0, Bound] or [Bound,0]
            gt | gt `elem` [GT, GE] -> do
              lower <- compareFloatExpr rt e2 (swap ord, min bound 0)
              higher <- compareFloatExpr rt e2 (swap ord, max bound 0)
              return $ higher - lower
            _ -> undefined
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      compareFloatExpr rt e1 (if c < 0 then swap ord else ord, value * c)
  | otherwise = Left "Can only interpret Divide(/) with a one side Constant."
compareFloatExpr rt (Exponent e1 e2) (ord, value)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      compareFloatExpr rt e1 (ord, value ** (1 / c))
  | otherwise = Left "Can only interpret Exponent(**) with a Constant."
compareFloatExpr rt (IfThenElse e1 e2 e3) (ord, value) = do
  (_dim, probTrue) <- interpret rt e1 (VBool True)
  let probFalse = 1 - probTrue
  p2 <- compareFloatExpr rt e2 (ord, value)
  p3 <- compareFloatExpr rt e3 (ord, value)
  return $ probTrue * p2 + probFalse * p3
compareFloatExpr rt (FnCall fnName arguments) (ord, value) = do
  expr <- justOr (lookup fnName (program rt)) ("Fn '" ++ fnName ++ "' not found.")
  let newDepth = 1 + recursionDepth rt
  let newRt = rt {recursionDepth = newDepth, arguments}
  if newDepth >= maxRecursionDepth rt then
    Right 0.0
  else
    compareFloatExpr newRt expr (ord, value)
compareFloatExpr rt (FnParameter index) (ord,value)
  | Just ele <- getElem (arguments rt) index = compareFloatExpr rt ele (ord, value)
  | otherwise = error $ "Could not find Parameter with index: " ++ show index
compareFloatExpr _ expr _ = case expr of
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

