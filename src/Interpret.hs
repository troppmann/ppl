module Interpret
  (
    interpret
  )
where

import Representation
import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Distribution.Uniform
import GHC.Float (float2Double, double2Float)

interpret :: Expr -> Value -> Probability
interpret Normal (VBool _) = error "Expected Float got Bool."
interpret Normal (VFloat f) = double2Float $ density distr $ float2Double f
    where distr = normalDistr 0.0 1.0
interpret Uniform (VBool _) = error "Expected Float got Bool."
interpret Uniform (VFloat f) = double2Float $ density distr $ float2Double f
    where distr = uniformDistr 0.0 1.0
interpret (Plus Uniform (Const (VFloat c))) (VFloat f) = double2Float $ density distr $ float2Double f
    where distr = uniformDistr (0.0 + float2Double c) (1.0 + float2Double c)
interpret (Plus (Const (VFloat c)) Uniform ) (VFloat f) = double2Float $ density distr $ float2Double f
    where distr = uniformDistr (0.0 + float2Double c) (1.0 + float2Double c)
interpret (Multiply (Const (VFloat c)) Uniform ) (VFloat f) = double2Float $ density distr $ float2Double f
    where distr = uniformDistr (0.0 * float2Double c) (1.0 * float2Double c)
interpret (Multiply Uniform (Const (VFloat c))) (VFloat f) = double2Float $ density distr $ float2Double f
    where distr = uniformDistr (0.0 * float2Double c) (1.0 * float2Double c)
interpret _ _ = todo


todo :: a
todo = error "not yet implemented"