module Sample
  ( sampleIO,
  )
where

import Representation
import Statistics.Distribution
import Statistics.Distribution.Normal
import System.Random

sampleIO :: Expr -> IO Value
sampleIO (Const v) = return v
sampleIO Uniform = do
  rValue <- randomRIO (0, 1) :: IO Float
  return $ VFloat rValue
sampleIO Normal = do
  rValue <- randomRIO (0, 1) :: IO Double
  let normal = normalDistr 0.0 1.0
  let nValue = realToFrac (quantile normal rValue) :: Float
  return $ VFloat nValue
sampleIO (Plus e1 e2) = apply (evaluateArithmetic (+)) e1 e2
sampleIO (Multiply e1 e2) = apply (evaluateArithmetic (*)) e1 e2
sampleIO (Subtract e1 e2) = apply (evaluateArithmetic (-)) e1 e2
sampleIO (Divide e1 e2) = apply (evaluateArithmetic (/)) e1 e2
sampleIO (And e1 e2) = sampleAnd e1 e2
sampleIO (Or e1 e2) = sampleOr e1 e2
sampleIO (Equal e1 e2) = apply (evaluateCompare (==)) e1 e2
sampleIO (Unequal e1 e2) = apply (evaluateCompare (/=)) e1 e2
sampleIO (LessThan e1 e2) = apply (evaluateCompare (<)) e1 e2
sampleIO (LessEqualThan e1 e2) = apply (evaluateCompare (<=)) e1 e2
sampleIO (GreaterThan e1 e2) = apply (evaluateCompare (>)) e1 e2
sampleIO (GreaterEqualThan e1 e2) = apply (evaluateCompare (>=)) e1 e2
sampleIO (IfElseThen e1 e2 e3) = sampleIfElse e1 e2 e3

apply :: (Value -> Value -> Value) -> Expr -> Expr -> IO Value
apply f e1 e2 = do
  v1 <- sampleIO e1
  v2 <- sampleIO e2
  return $ f v1 v2

sampleIfElse :: Expr -> Expr -> Expr -> IO Value
sampleIfElse e1 e2 e3 = do
  v1 <- sampleIO e1
  if evaluateAsBool v1
    then
      sampleIO e2
    else
      sampleIO e3

sampleAnd :: Expr -> Expr -> IO Value
sampleAnd e1 e2 = do
  v1 <- sampleIO e1
  if evaluateAsBool v1
    then do
      v2 <- sampleIO e2
      return $ VBool $ evaluateAsBool v2
    else
      return $ VBool False

sampleOr :: Expr -> Expr -> IO Value
sampleOr e1 e2 = do
  v1 <- sampleIO e1
  if evaluateAsBool v1
    then
      return $ VBool True
    else do
      v2 <- sampleIO e2
      return $ VBool $ evaluateAsBool v2

evaluateAsBool :: Value -> Bool
evaluateAsBool (VFloat _) = error "Error: Expected Bool got Float."
evaluateAsBool (VBool b) = b

evaluateArithmetic :: (Float -> Float -> Float) -> Value -> Value -> Value
evaluateArithmetic _ (VBool _) _ = error "Error: Can't calculate a Boolean Value."
evaluateArithmetic _ _ (VBool _) = error "Error: Can't calculate a Boolean Value."
evaluateArithmetic f (VFloat x) (VFloat y) = VFloat $ f x y

evaluateCompare :: (forall a. (Eq a, Ord a) => a -> a -> Bool) -> Value -> Value -> Value
evaluateCompare _ (VBool _) (VFloat _) = error "Error: Can't compare Bool with Float."
evaluateCompare _ (VFloat _) (VBool _) = error "Error: Can't compare Float with Bool."
evaluateCompare f (VFloat x) (VFloat y) = VBool $ f x y
evaluateCompare f (VBool x) (VBool y) = VBool $ f x y