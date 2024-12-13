module Sample
  ( sampleExpr,
    sampleRand,
  )
where

import Control.Monad.Random (MonadRandom, evalRandIO, getRandomR)
import Representation (Expr (..), Value (..))
import Statistics.Distribution
import Statistics.Distribution.Normal (normalDistr)

-- TODO 11.09.24: Make sampleExpr/sampleRand a IO Either String Value return type
sampleExpr :: Expr -> IO Value
sampleExpr expr = evalRandIO (sampleRand expr)

sampleRand :: (MonadRandom m) => Expr -> m Value
sampleRand (Const v) = return v
sampleRand Uniform = do
  rValue <- getRandomR (0.0, 1.0)
  return $ VFloat rValue
sampleRand Normal = do
  rValue <- getRandomR (0, 1)
  let normal = normalDistr 0.0 1.0
  let nValue = quantile normal rValue
  return $ VFloat nValue
sampleRand (Plus e1 e2) = apply (evaluateArithmetic (+)) e1 e2
sampleRand (Exponent e1 e2) = apply (evaluateArithmetic (**)) e1 e2
sampleRand (Multiply e1 e2) = apply (evaluateArithmetic (*)) e1 e2
sampleRand (Subtract e1 e2) = apply (evaluateArithmetic (-)) e1 e2
sampleRand (Divide e1 e2) = apply (evaluateArithmetic (/)) e1 e2
sampleRand (And e1 e2) = sampleAnd e1 e2
sampleRand (Or e1 e2) = sampleOr e1 e2
sampleRand (Not expr) = sampleNot expr
sampleRand (Equal e1 e2) = apply (evaluateCompare (==)) e1 e2
sampleRand (Unequal e1 e2) = apply (evaluateCompare (/=)) e1 e2
sampleRand (LessThan e1 e2) = apply (evaluateCompare (<)) e1 e2
sampleRand (LessThanOrEqual e1 e2) = apply (evaluateCompare (<=)) e1 e2
sampleRand (GreaterThan e1 e2) = apply (evaluateCompare (>)) e1 e2
sampleRand (GreaterThanOrEqual e1 e2) = apply (evaluateCompare (>=)) e1 e2
sampleRand (IfThenElse e1 e2 e3) = sampleIfElse e1 e2 e3
sampleRand (CreateTuple e1 e2) = sampleTuple e1 e2

apply :: (MonadRandom m) => (Value -> Value -> Value) -> Expr -> Expr -> m Value
apply f e1 e2 = do
  v1 <- sampleRand e1
  v2 <- sampleRand e2
  return $ f v1 v2

sampleIfElse :: (MonadRandom m) => Expr -> Expr -> Expr -> m Value
sampleIfElse e1 e2 e3 = do
  v1 <- sampleRand e1
  if evaluateAsBool v1
    then
      sampleRand e2
    else
      sampleRand e3

sampleAnd :: (MonadRandom m) => Expr -> Expr -> m Value
sampleAnd e1 e2 = do
  v1 <- sampleRand e1
  if evaluateAsBool v1
    then do
      v2 <- sampleRand e2
      return $ VBool $ evaluateAsBool v2
    else
      return $ VBool False

sampleOr :: (MonadRandom m) => Expr -> Expr -> m Value
sampleOr e1 e2 = do
  v1 <- sampleRand e1
  if evaluateAsBool v1
    then
      return $ VBool True
    else do
      v2 <- sampleRand e2
      return $ VBool $ evaluateAsBool v2

sampleNot :: (MonadRandom m) => Expr -> m Value
sampleNot expr = do
  value <- sampleRand expr
  return $ VBool $ not $ evaluateAsBool value

sampleTuple :: (MonadRandom m) => Expr -> Expr -> m Value
sampleTuple e1 e2 = do
  v1 <- sampleRand e1
  v2 <- sampleRand e2
  return $ VTuple v1 v2

evaluateAsBool :: Value -> Bool
evaluateAsBool (VBool b) = b
evaluateAsBool v1 = error $ "Error: Expected Bool got " <> show v1 <> " ."

evaluateArithmetic :: (Double -> Double -> Double) -> Value -> Value -> Value
evaluateArithmetic f (VFloat x) (VFloat y) = VFloat $ f x y
evaluateArithmetic _ v1 v2 = error $ "Error: Can't calculate " <> show v1 <> " with " <> show v2 <> "."

evaluateCompare :: (forall a. (Eq a, Ord a) => a -> a -> Bool) -> Value -> Value -> Value
evaluateCompare f (VFloat x) (VFloat y) = VBool $ f x y
evaluateCompare f (VBool x) (VBool y) = VBool $ f x y
evaluateCompare _ v1 v2 = error $ "Error: Can't compare " <> show v1 <> " with " <> show v2 <> "."