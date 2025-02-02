module Infer
  ( inferProgram,
    infer,
  )
where

import Control.Monad ((<=<))
import Debug.Extended
import Evaluate
import Optimizer (optimizeExpr)
import Representation
import Runtime
import Statistics.Distribution
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Uniform
import Prelude hiding (EQ, GT, LT)
import Control.Exception (assert)

type ErrorString = String

inferProgram :: Program -> Value -> Either ErrorString DimensionalProbability
inferProgram program value = do
  mainExpr <- justOr (lookup "main" program) "main-Func not found."
  let runTime = Runtime {program, arguments = [], currentFnName = "main", recursionDepth = 0, maxRecursionDepth = 100}
  infer runTime mainExpr value

infer :: Runtime -> Expr -> Value -> Either String DimensionalProbability
infer _ Uniform (VFloat v) = return (1, if 0.0 <= v && v < 1.0 then 1.0 else 0.0)
infer _ Normal (VFloat v) = return (1, density distr v)
  where
    distr = normalDistr 0.0 1.0
infer _ (Const v1) value = return (0, if v1 == value then 1.0 else 0.0)
infer rt (Plus e1 e2) value@(VFloat _)
  | Right constant <- evalConstExpr rt e1 = do
      newValue <- evalArithmetic (-) value constant
      infer rt e2 newValue
  | Right constant <- evalConstExpr rt e2 = do
      newValue <- evalArithmetic (-) value constant
      infer rt e1 newValue
  | otherwise = Left "Can only infer Plus(+) with a one side Constant."
infer rt (Subtract e1 e2) value@(VFloat _)
  | Right constant <- evalConstExpr rt e1 = do
      newValue <- evalArithmetic (-) constant value
      infer rt e2 newValue
  | Right constant <- evalConstExpr rt e2 = do
      newValue <- evalArithmetic (+) value constant
      infer rt e1 newValue
  | otherwise = Left "Can only infer Subtract(-) with a one side Constant."
infer rt (Multiply e1 e2) value@(VFloat v)
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      if c == 0.0
        then
          infer rt (Const $ VFloat 0.0) value
        else do
          (dim, prob) <- infer rt e2 (VFloat $ v / c)
          case dim of
            0 -> return (0, prob)
            _ -> return (1, prob / abs c)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      if c == 0.0
        then
          infer rt (Const $ VFloat 0.0) value
        else do
          (dim, prob) <- infer rt e1 (VFloat $ v / c)
          case dim of
            0 -> return (0, prob)
            _ -> return (1, prob / abs c)
  | otherwise = Left "Can only infer Multiply(*) with a one side Constant."
infer rt (Divide e1 e2) (VFloat v)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      (dim, prob) <- infer rt e1 (VFloat $ v * c)
      case dim of
        0 -> return (0, prob)
        _ -> return (1, prob * abs c)
  | Right constant <- evalConstExpr rt e1 = do
      -- TODO works only on monotone functions and c != 0.0 and v != 0.0
      c <- evalAsFloat constant
      (dim, prob) <- infer rt e2 (VFloat $ c / v)
      case dim of
        0 -> return (0, prob)
        _ -> return (1, prob * abs (c / (v * v)))
  | otherwise = Left "Can only infer Divide(/) with a one side Constant."
infer rt (Exponent e1 e2) value@(VFloat v)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      if c == 0.0
        then
          infer rt (Const $ VFloat 1.0) value
        else do
          let overC = 1 / c
          (dim, prob) <- infer rt e1 (VFloat $ signum v * (v ** overC))
          case dim of
            0 -> return (0, prob)
            _ -> return (1, prob * abs (overC * (v ** (overC - 1))))
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      case c of
        1.0 -> infer rt (Const $ VFloat 1.0) value
        0.0 -> infer rt (Const $ VFloat 0.0) value
        _ -> do
          (dim, prob) <- infer rt e2 (VFloat $ logBase c v)
          case dim of
            0 -> return (0, prob)
            _ -> return (1, prob * abs (1 / (v * log c)))
  | otherwise = Left "Can only infer Exponent(**) with a one side Constant."
infer rt (Abs expr) value@(VFloat v) =
  if v < 0
    then
      return (0, 0.0)
    else do
      plus <- infer rt expr value
      minus <- infer rt expr (VFloat (-v))
      return $ plus #+# minus
infer rt (IfThenElse e1 e2 e3) value = do
  dimProbTrue@(dimTrue, _) <- infer rt e1 (VBool True)
  dimProbFalse@(dimFalse, _) <- infer rt e1 (VBool False) -- (0, 1.0) #-# dimProbTrue
  if dimProbTrue == (0, 1.0) && dimFalse == 0
    then do
      infer rt e2 value
    else
      if dimProbFalse == (0, 1.0) && dimTrue == 0
        then do
          infer rt e3 value
        else do
          dimProbBranchTrue <- infer rt e2 value
          dimProbBranchFalse <- infer rt e3 value
          return $ (dimProbTrue #*# dimProbBranchTrue) #+# (dimProbFalse #*# dimProbBranchFalse)
infer rt (Equal e1 e2) (VBool bool)
  | Right constant <- evalConstExpr rt e2 = do
      dimProb <- infer rt e1 constant
      return $ if bool then dimProb else (0, 1.0) #-# dimProb
  | Right constant <- evalConstExpr rt e1 = do
      dimProb <- infer rt e2 constant
      return $ if bool then dimProb else (0, 1.0) #-# dimProb
  | otherwise = Left "Can only infer == with a one side Constant."
infer rt (Unequal e1 e2) (VBool bool) = infer rt (Equal e1 e2) (VBool $ not bool)
infer rt (LessThanOrEqual e1 e2) (VBool bool)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e1 (LE, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e2 (GE, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only infer <= with a one side Constant."
infer rt (LessThan e1 e2) (VBool bool)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e1 (LT, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e2 (GT, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only infer <= with a one side Constant."
infer rt (GreaterThan e1 e2) (VBool bool)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e1 (GT, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e2 (LT, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only infer > with a one side Constant."
infer rt (GreaterThanOrEqual e1 e2) (VBool bool)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e1 (GE, c)
      return (0, if bool then x else 1 - x)
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      x <- compareFloatExpr rt e2 (LE, c)
      return (0, if bool then x else 1 - x)
  | otherwise = Left "Can only infer >= with a one side Constant."
infer rt (Or e1 e2) (VBool bool) = do
  dimProb1True <- infer rt e1 (VBool bool)
  let dimProb1False = (0, 1.0) #-# dimProb1True
  dimProb2True <- infer rt e2 (VBool bool)
  let dimProb2False = (0, 1.0) #-# dimProb2True
  -- double negative vanishes dimensions over 0
  -- return $ (0, 1.0) #-# ((0, 1.0) #-# dimProb1True) #*# ((0, 1.0) #-# dimProb2True)
  return $ (dimProb1True #*# dimProb2True) #+# (dimProb1True #*# dimProb2False) #+# (dimProb1False #*# dimProb2True)
infer rt (And e1 e2) (VBool bool) = do
  dimProb1 <- infer rt e1 (VBool bool)
  dimProb2 <- infer rt e2 (VBool bool)
  return $ dimProb1 #*# dimProb2
infer rt (Not e1) (VBool bool) = infer rt e1 (VBool $ not bool)
infer rt (CreateTuple e1 e2) (VTuple v1 v2) = do
  dimProbA <- infer rt e1 v1
  dimProbB <- infer rt e2 v2
  return $ dimProbA #*# dimProbB
infer rt (FnCall fnName arguments) val = do
  expr <- justOr (lookup fnName (program rt)) ("Fn '" ++ fnName ++ "' not found.")
  let newDepth = 1 + recursionDepth rt
  args <- traverse (optimizeExpr rt {recursionDepth= 0,maxRecursionDepth = 1} <=< replaceFnParameterWithContent rt) arguments
  let newRt = rt {recursionDepth = newDepth, arguments = args}
  if recursionDepth rt >= maxRecursionDepth rt
    then
      return (0, 0.0)
    else
      infer newRt expr val
infer rt (FnParameter index) val
  | Just ele <- getElem (arguments rt) index = infer rt ele val
  | otherwise = error $ "Could not find Parameter with index: " ++ show index
infer _ (Custom (InferFn fn) _ _) val = return $ fn val
-- In all other cases is the requested value not the right output.
-- The catch all case _ is not used.
-- This is explicit so that not all new Expr are auto implemented.
infer _ Uniform (VBool _) = return (0, 0.0)
infer _ Normal (VBool _) = return (0, 0.0)
infer _ (Plus _ _) (VBool _) = return (0, 0.0)
infer _ (Subtract _ _) (VBool _) = return (0, 0.0)
infer _ (Multiply _ _) (VBool _) = return (0, 0.0)
infer _ (Divide _ _) (VBool _) = return (0, 0.0)
infer _ (Exponent _ _) (VBool _) = return (0, 0.0)
infer _ (CreateTuple _ _) (VBool _) = return (0, 0.0)
infer _ (Abs _) (VBool _) = return (0, 0.0)
infer _ (Not _) (VFloat _) = return (0, 0.0)
infer _ (And _ _) (VFloat _) = return (0, 0.0)
infer _ (Or _ _) (VFloat _) = return (0, 0.0)
infer _ (Equal _ _) (VFloat _) = return (0, 0.0)
infer _ (Unequal _ _) (VFloat _) = return (0, 0.0)
infer _ (LessThan _ _) (VFloat _) = return (0, 0.0)
infer _ (LessThanOrEqual _ _) (VFloat _) = return (0, 0.0)
infer _ (GreaterThan _ _) (VFloat _) = return (0, 0.0)
infer _ (GreaterThanOrEqual _ _) (VFloat _) = return (0, 0.0)
infer _ (CreateTuple _ _) (VFloat _) = return (0, 0.0)
infer _ Uniform (VTuple _ _) = return (0, 0.0)
infer _ Normal (VTuple _ _) = return (0, 0.0)
infer _ (Plus _ _) (VTuple _ _) = return (0, 0.0)
infer _ (Subtract _ _) (VTuple _ _) = return (0, 0.0)
infer _ (Multiply _ _) (VTuple _ _) = return (0, 0.0)
infer _ (Divide _ _) (VTuple _ _) = return (0, 0.0)
infer _ (Exponent _ _) (VTuple _ _) = return (0, 0.0)
infer _ (Abs _) (VTuple _ _) = return (0, 0.0)
infer _ (Not _) (VTuple _ _) = return (0, 0.0)
infer _ (And _ _) (VTuple _ _) = return (0, 0.0)
infer _ (Or _ _) (VTuple _ _) = return (0, 0.0)
infer _ (Equal _ _) (VTuple _ _) = return (0, 0.0)
infer _ (Unequal _ _) (VTuple _ _) = return (0, 0.0)
infer _ (LessThan _ _) (VTuple _ _) = return (0, 0.0)
infer _ (LessThanOrEqual _ _) (VTuple _ _) = return (0, 0.0)
infer _ (GreaterThan _ _) (VTuple _ _) = return (0, 0.0)
infer _ (GreaterThanOrEqual _ _) (VTuple _ _) = return (0, 0.0)



data CompareCase = LT | LE | GE | GT deriving (Ord, Enum, Show, Eq)

type CompareQuery = (CompareCase, Double)

swap :: CompareCase -> CompareCase
swap LT = GT
swap LE = GE
swap GT = LT
swap GE = LE

-- TODO 06.09.2024: Should i add a dimension differentiation?
compareFloatExpr :: Runtime -> Expr -> CompareQuery -> Either String Double
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
  | otherwise = Left "Can only infer Multiply(*) with a one side Constant."
compareFloatExpr rt (Subtract e1 e2) (ord, value)
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      compareFloatExpr rt e2 (swap ord, -value + c)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      compareFloatExpr rt e1 (ord, value + c)
  | otherwise = Left "Can only infer Subtract(-) with a one side Constant."
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
  | otherwise = Left "Can only infer Multiply(*) with a one side Constant."
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
  | otherwise = Left "Can only infer Divide(/) with a one side Constant."
compareFloatExpr rt (Exponent e1 e2) (ord, value)
  | Right constant <- evalConstExpr rt e2 = do
      c <- evalAsFloat constant
      compareFloatExpr rt e1 (ord, value ** (1 / c))
  | Right constant <- evalConstExpr rt e1 = do
      c <- evalAsFloat constant
      if value < 0 then 
        case ord of 
          lt | lt `elem` [LT, LE] -> return 0.0
          gt | gt `elem` [GT, GE] -> return 1.0
          _ -> undefined
      else 
        compareFloatExpr rt e2 (if c < 1 then swap ord else ord, logBase c value)
  | otherwise = Left "Can only infer Exponent(**) with a Constant."
compareFloatExpr rt (Abs expr) (ord, value) = do
  case ord of 
    lt | lt `elem` [LT, LE] -> do
      if value < 0 then
        return 0
      else do
        prob1 <- compareFloatExpr rt expr (ord, value)
        prob2 <- compareFloatExpr rt expr (ord, -value)
        return (prob1 - prob2)
    gt | gt `elem` [GT, GE] -> do
      if value < 0 then
        return 1
      else do
        prob1 <- compareFloatExpr rt expr (swap ord, -value)
        prob2 <- compareFloatExpr rt expr (ord, value)
        return (prob1 + prob2)
    _ -> undefined
compareFloatExpr rt (IfThenElse e1 e2 e3) (ord, value) = do
  dimProbTrue <- infer rt e1 (VBool True)
  dimProbFalse <- infer rt e1 (VBool False)
  if dimProbTrue == (0, 1.0)
    then
      compareFloatExpr rt e2 (ord, value)
    else
      if dimProbFalse == (0, 1.0)
        then
          compareFloatExpr rt e3 (ord, value)
        else do
          p2 <- compareFloatExpr rt e2 (ord, value)
          p3 <- compareFloatExpr rt e3 (ord, value)
          let (dim, prob) = dimProbTrue #*# (0, p2) #+# dimProbFalse #*# (0, p3)
          return $ assert (dim == 0) prob
compareFloatExpr rt (FnCall fnName arguments) (ord, value) = do
  expr <- justOr (lookup fnName (program rt)) ("Fn '" ++ fnName ++ "' not found.")
  let newDepth = 1 + recursionDepth rt
  args <- traverse (optimizeExpr rt {maxRecursionDepth = 0} <=< replaceFnParameterWithContent rt) arguments
  let newRt = rt {recursionDepth = newDepth, arguments = args}
  if recursionDepth rt >= maxRecursionDepth rt
    then
      return 0.0
    else
      compareFloatExpr newRt expr (ord, value)
compareFloatExpr rt (FnParameter index) (ord, value)
  | Just ele <- getElem (arguments rt) index = compareFloatExpr rt ele (ord, value)
  | otherwise = error $ "Could not find Parameter with index: " ++ show index
compareFloatExpr _ (Custom _ (CumulativeFn cumulativeFn) _) (ord, value) = return $ case ord of
  LT -> cumulativeFn value
  LE -> cumulativeFn value
  GT -> 1 - cumulativeFn value
  GE -> 1 - cumulativeFn value
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
