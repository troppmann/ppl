module Sample
  ( sample,
  )
where

import Parser

sample :: Expr -> Value
sample (Const v) = v
sample (Plus fExpr sExpr) = samplePlus (sample fExpr) (sample sExpr)
sample (Multiply fExpr sExpr) = sampleMultiply (sample fExpr) (sample sExpr)
sample (Subtract fExpr sExpr) = sampleSubtract (sample fExpr) (sample sExpr)

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