module Sample
  ( sampleExpr,
    sampleRand,
    sampleProgram,
    SampleRuntime(..),
    defaultSampleRuntime,
  )
where

import Control.Monad.Random
import Debug.Extended
import Representation
import Statistics.Distribution
import Statistics.Distribution.Normal (normalDistr)

sampleProgram :: Program -> IO Value
sampleProgram program = evalRandIO $ sampleRand rt mainExpr
  where
    rt = defaultSampleRuntime program
    mainExpr = unwrapMaybe $ lookup "main" program

sampleExpr :: Expr -> IO Value
sampleExpr expr = todo "Wrap in main"

defaultSampleRuntime :: Program -> SampleRuntime
defaultSampleRuntime program = SampleRuntime {program, arguments = [], recursionDepth = 0, maxRecursionDepth = 10000} 

data SampleRuntime = SampleRuntime
  { program :: Program,
    arguments :: [Value],
    recursionDepth :: Int,
    maxRecursionDepth :: Int
  }
  deriving (Show, Eq)

sampleRand :: (MonadRandom m) => SampleRuntime -> Expr -> m Value
sampleRand _ (Const v) = return v
sampleRand _ Uniform = do
  rValue <- getRandomR (0.0, 1.0)
  return $ VFloat rValue
sampleRand _ Normal = do
  rValue <- getRandomR (0, 1)
  let normal = normalDistr 0.0 1.0
  let nValue = quantile normal rValue
  return $ VFloat nValue
sampleRand rt (Plus e1 e2) = apply rt (evaluateArithmetic (+)) e1 e2
sampleRand rt (Exponent e1 e2) = apply rt (evaluateArithmetic (**)) e1 e2
sampleRand rt (Multiply e1 e2) = apply rt (evaluateArithmetic (*)) e1 e2
sampleRand rt (Subtract e1 e2) = apply rt (evaluateArithmetic (-)) e1 e2
sampleRand rt (Divide e1 e2) = apply rt (evaluateArithmetic (/)) e1 e2
sampleRand rt (And e1 e2) = sampleAnd rt e1 e2
sampleRand rt (Or e1 e2) = sampleOr rt e1 e2
sampleRand rt (Not expr) = sampleNot rt expr
sampleRand rt (Equal e1 e2) = apply rt (evaluateCompare (==)) e1 e2
sampleRand rt (Unequal e1 e2) = apply rt (evaluateCompare (/=)) e1 e2
sampleRand rt (LessThan e1 e2) = apply rt (evaluateCompare (<)) e1 e2
sampleRand rt (LessThanOrEqual e1 e2) = apply rt (evaluateCompare (<=)) e1 e2
sampleRand rt (GreaterThan e1 e2) = apply rt (evaluateCompare (>)) e1 e2
sampleRand rt (GreaterThanOrEqual e1 e2) = apply rt (evaluateCompare (>=)) e1 e2
sampleRand rt (IfThenElse e1 e2 e3) = sampleIfElse rt e1 e2 e3
sampleRand rt (CreateTuple e1 e2) = sampleTuple rt e1 e2
sampleRand rt (FnCall fnName _arguments) = do
  case lookup fnName (program rt ) of
    (Just expr) -> do 
      let newDepth = 1 + recursionDepth rt
      let newRt = rt {recursionDepth = newDepth}
      if newDepth >= maxRecursionDepth rt then
        error $ "Max Recursion Depth reached: " ++ show (maxRecursionDepth rt)
      else
        sampleRand newRt expr 
    Nothing -> error $ "Could not find FnName: " <> fnName <> " ."

apply :: (MonadRandom m) => SampleRuntime -> (Value -> Value -> Value) -> Expr -> Expr -> m Value
apply rt f e1 e2 = do
  v1 <- sampleRand rt e1
  v2 <- sampleRand rt e2
  return $ f v1 v2

sampleIfElse :: (MonadRandom m) => SampleRuntime -> Expr -> Expr -> Expr -> m Value
sampleIfElse rt e1 e2 e3 = do
  v1 <- sampleRand rt e1
  if evaluateAsBool v1
    then
      sampleRand rt e2
    else
      sampleRand rt e3

sampleAnd :: (MonadRandom m) => SampleRuntime -> Expr -> Expr -> m Value
sampleAnd rt e1 e2 = do
  v1 <- sampleRand rt e1
  if evaluateAsBool v1
    then do
      v2 <- sampleRand rt e2
      return $ VBool $ evaluateAsBool v2
    else
      return $ VBool False

sampleOr :: (MonadRandom m) => SampleRuntime -> Expr -> Expr -> m Value
sampleOr rt e1 e2 = do
  v1 <- sampleRand rt e1
  if evaluateAsBool v1
    then
      return $ VBool True
    else do
      v2 <- sampleRand rt e2
      return $ VBool $ evaluateAsBool v2

sampleNot :: (MonadRandom m) => SampleRuntime ->  Expr -> m Value
sampleNot rt expr = do
  value <- sampleRand rt expr
  return $ VBool $ not $ evaluateAsBool value

sampleTuple :: (MonadRandom m) => SampleRuntime ->  Expr -> Expr -> m Value
sampleTuple rt e1 e2 = do
  v1 <- sampleRand rt e1
  v2 <- sampleRand rt e2
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