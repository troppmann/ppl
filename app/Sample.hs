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
sampleIO (Plus fExpr sExpr) = do
  fValue <- sampleIO fExpr
  sValue <- sampleIO sExpr
  return $ samplePlus fValue sValue
sampleIO (Multiply fExpr sExpr) = do
  fValue <- sampleIO fExpr
  sValue <- sampleIO sExpr
  return $ sampleMultiply fValue sValue
sampleIO (Subtract fExpr sExpr) = do
  fValue <- sampleIO fExpr
  sValue <- sampleIO sExpr
  return $ sampleSubtract fValue sValue

samplePlus :: Value -> Value -> Value
samplePlus (VBool _) _ = error "Error: Can't add a Boolean Value."
samplePlus _ (VBool _) = error "Error: Can't add a Boolean Value."
samplePlus (VFloat x) (VFloat y) = VFloat $ x + y

sampleMultiply :: Value -> Value -> Value
sampleMultiply (VBool _) _ = error "Error: Can't multiply a Boolean Value."
sampleMultiply _ (VBool _) = error "Error: Can't multiply a Boolean Value."
sampleMultiply (VFloat x) (VFloat y) = VFloat $ x * y

sampleSubtract :: Value -> Value -> Value
sampleSubtract (VBool _) _ = error "Error: Can't subtract a Boolean Value."
sampleSubtract _ (VBool _) = error "Error: Can't subtract a Boolean Value."
sampleSubtract (VFloat x) (VFloat y) = VFloat $ x - y