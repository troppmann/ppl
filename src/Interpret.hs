module Interpret
  ( interpret,
  )
where

import Evaluate
import Representation
import Statistics.Distribution
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Uniform

interpret :: Expr -> Value -> Either String Probability
interpret Uniform (VFloat f) = Right $ density distr f
  where
    distr = uniformDistr 0.0 1.0
interpret Normal (VFloat f) = Right $ density distr f
  where
    distr = normalDistr 0.0 1.0
interpret (Const v1) value = if v1 == value then Right 1.0 else Right 0.0
interpret (Plus e1 e2) value
  | Right constant <- evalConstExpr e1 = do
      newValue <- evalArithmetic (-) value constant
      interpret e2 newValue
  | Right constant <- evalConstExpr e2 = do
      newValue <- evalArithmetic (-) value constant
      interpret e1 newValue
  | otherwise = Left "Can only interpret Plus(+) with a one side Constant."
interpret (Subtract e1 e2) value
  | Right constant <- evalConstExpr e1 = do
      newValue <- evalArithmetic (-) constant value
      interpret e2 newValue
  | Right constant <- evalConstExpr e2 = do
      newValue <- evalArithmetic (+) value constant
      interpret e1 newValue
  | otherwise = Left "Can only interpret Subtract(+) with a one side Constant."
interpret (Multiply e1 e2) value
  | Right constant <- evalConstExpr e1 = do
      c <- evalAsFloat constant
      if c == 0.0
        then
          interpret (Const $ VFloat 0.0) value
        else do
          v <- evalAsFloat value
          f <- interpret e2 (VFloat $ v / c)
          return (f / abs c)
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      if c == 0.0
        then
          interpret (Const $ VFloat 0.0) value
        else do
          v <- evalAsFloat value
          f <- interpret e1 (VFloat $ v / c)
          return (f / abs c)
  | otherwise = Left "Can only interpret Multiply(+) with a one side Constant."
interpret (Divide e1 e2) value
  | Right constant <- evalConstExpr e1 = do
      -- TODO works only on monotone uniforms and c != 0.0 and v != 0.0
      c <- evalAsFloat constant
      v <- evalAsFloat value
      f <- interpret e2 (VFloat $ c / v)
      return $ f * abs ((-c) / (v * v))
  | Right constant <- evalConstExpr e2 = do
      c <- evalAsFloat constant
      v <- evalAsFloat value
      f <- interpret e1 (VFloat $ v * c)
      return (f * abs c)
  | otherwise = Left "Can only interpret Divide(/) with a one side Constant."
interpret _ _ = todo "Missing case."

todo :: String -> a
todo msg = error ("not yet implemented: " ++ msg)