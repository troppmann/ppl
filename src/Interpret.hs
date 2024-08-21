module Interpret
  ( interpret,
  )
where

import Representation
import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Distribution.Uniform

interpret :: Expr -> Value -> Probability
interpret Normal (VBool _) = error "Expected Float got Bool."
interpret Normal (VFloat f) = density distr f
  where
    distr = normalDistr 0.0 1.0
interpret Uniform (VBool _) = error "Expected Float got Bool."
interpret Uniform (VFloat f) = density distr f
  where
    distr = uniformDistr 0.0 1.0
interpret (Plus Uniform (Const (VFloat c))) (VFloat f) = density distr f
  where
    distr = uniformDistr (0.0 + c) (1.0 + c)
interpret (Plus (Const (VFloat c)) Uniform) (VFloat f) = density distr f
  where
    distr = uniformDistr (0.0 + c) (1.0 + c)
interpret (Multiply (Const (VFloat c)) Uniform) (VFloat f) = density distr f
  where
    distr = uniformDistr (0.0 * c) (1.0 * c)
interpret (Multiply Uniform (Const (VFloat c))) (VFloat f) = density distr f
  where
    distr = uniformDistr (0.0 * c) (1.0 * c)
interpret _ _ = todo

todo :: a
todo = error "not yet implemented"