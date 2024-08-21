module Sample
  ( sampleIO,
  )
where

import Control.Monad.Random (Rand, RandomGen, getRandomR)
import Representation
import Statistics.Distribution
import Statistics.Distribution.Normal

sampleIO :: (RandomGen g) => Expr -> Rand g Value
sampleIO (Const v) = return v
sampleIO Uniform = do
  rValue <- getRandomR (0.0, 1.0)
  return $ VFloat rValue
sampleIO Normal = do
  rValue <- getRandomR (0, 1)
  let normal = normalDistr 0.0 1.0
  let nValue = quantile normal rValue
  return $ VFloat nValue
sampleIO (Plus e1 e2) = apply (evaluateArithmetic (+)) e1 e2
sampleIO (Multiply e1 e2) = apply (evaluateArithmetic (*)) e1 e2
sampleIO (Subtract e1 e2) = apply (evaluateArithmetic (-)) e1 e2
sampleIO (Divide e1 e2) = apply (evaluateArithmetic (/)) e1 e2
sampleIO (And e1 e2) = sampleAnd e1 e2
sampleIO (Or e1 e2) = sampleOr e1 e2
sampleIO (Not expr) = sampleNot expr
sampleIO (Equal e1 e2) = apply (evaluateCompare (==)) e1 e2
sampleIO (Unequal e1 e2) = apply (evaluateCompare (/=)) e1 e2
sampleIO (LessThan e1 e2) = apply (evaluateCompare (<)) e1 e2
sampleIO (LessEqualThan e1 e2) = apply (evaluateCompare (<=)) e1 e2
sampleIO (GreaterThan e1 e2) = apply (evaluateCompare (>)) e1 e2
sampleIO (GreaterEqualThan e1 e2) = apply (evaluateCompare (>=)) e1 e2
sampleIO (IfElseThen e1 e2 e3) = sampleIfElse e1 e2 e3

apply :: (RandomGen g) => (Value -> Value -> Value) -> Expr -> Expr -> Rand g Value
apply f e1 e2 = do
  v1 <- sampleIO e1
  v2 <- sampleIO e2
  return $ f v1 v2

sampleIfElse :: (RandomGen g) => Expr -> Expr -> Expr -> Rand g Value
sampleIfElse e1 e2 e3 = do
  v1 <- sampleIO e1
  if evaluateAsBool v1
    then
      sampleIO e2
    else
      sampleIO e3

sampleAnd :: (RandomGen g) => Expr -> Expr -> Rand g Value
sampleAnd e1 e2 = do
  v1 <- sampleIO e1
  if evaluateAsBool v1
    then do
      v2 <- sampleIO e2
      return $ VBool $ evaluateAsBool v2
    else
      return $ VBool False

sampleOr :: (RandomGen g) => Expr -> Expr -> Rand g Value
sampleOr e1 e2 = do
  v1 <- sampleIO e1
  if evaluateAsBool v1
    then
      return $ VBool True
    else do
      v2 <- sampleIO e2
      return $ VBool $ evaluateAsBool v2

sampleNot :: (RandomGen g) => Expr -> Rand g Value
sampleNot expr = do
  value <- sampleIO expr
  return $ VBool $ not $ evaluateAsBool value

evaluateAsBool :: Value -> Bool
evaluateAsBool (VFloat _) = error "Error: Expected Bool got Float."
evaluateAsBool (VBool b) = b

evaluateArithmetic :: (Double -> Double -> Double) -> Value -> Value -> Value
evaluateArithmetic _ (VBool _) _ = error "Error: Can't calculate a Boolean Value."
evaluateArithmetic _ _ (VBool _) = error "Error: Can't calculate a Boolean Value."
evaluateArithmetic f (VFloat x) (VFloat y) = VFloat $ f x y

evaluateCompare :: (forall a. (Eq a, Ord a) => a -> a -> Bool) -> Value -> Value -> Value
evaluateCompare _ (VBool _) (VFloat _) = error "Error: Can't compare Bool with Float."
evaluateCompare _ (VFloat _) (VBool _) = error "Error: Can't compare Float with Bool."
evaluateCompare f (VFloat x) (VFloat y) = VBool $ f x y
evaluateCompare f (VBool x) (VBool y) = VBool $ f x y