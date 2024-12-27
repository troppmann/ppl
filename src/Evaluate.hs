module Evaluate
  ( evalConstExpr,
    evalArithmetic,
    evalAsBool,
    evalAsFloat,
    evalCompare,
  )
where

import Representation
import Runtime
import Debug.Extended

type ErrorString = String

type ResultValue = Either ErrorString Value

evalConstExpr :: Runtime -> Expr -> ResultValue
evalConstExpr _ (Const v) = Right v
evalConstExpr _ Normal = Left "Normal is not a constant."
evalConstExpr _ Uniform = Left "Uniform is not a constant."
evalConstExpr rt (Plus e1 e2) = apply rt (evalArithmetic (+)) e1 e2
evalConstExpr rt (Multiply e1 e2) = apply rt (evalArithmetic (*)) e1 e2
evalConstExpr rt (Subtract e1 e2) = apply rt (evalArithmetic (-)) e1 e2
evalConstExpr rt (Divide e1 e2) = apply rt (evalArithmetic (/)) e1 e2
evalConstExpr rt (Exponent e1 e2) = apply rt (evalArithmetic (**)) e1 e2
evalConstExpr rt (And e1 e2) = evalAnd rt e1 e2
evalConstExpr rt (Or e1 e2) = evalOr rt e1 e2
evalConstExpr rt (Not expr) = evalNot rt expr
evalConstExpr rt (Equal e1 e2) = apply rt (evalCompare (==)) e1 e2
evalConstExpr rt (Unequal e1 e2) = apply rt (evalCompare (/=)) e1 e2
evalConstExpr rt (LessThan e1 e2) = apply rt (evalCompare (<)) e1 e2
evalConstExpr rt (LessThanOrEqual e1 e2) = apply rt (evalCompare (<=)) e1 e2
evalConstExpr rt (GreaterThan e1 e2) = apply rt (evalCompare (>)) e1 e2
evalConstExpr rt (GreaterThanOrEqual e1 e2) = apply rt (evalCompare (>=)) e1 e2
evalConstExpr rt (IfThenElse e1 e2 e3) = evalIfThenElse rt e1 e2 e3
evalConstExpr rt (CreateTuple e1 e2) = evalCreateTuple rt e1 e2
evalConstExpr _ (FnCall _ _) = Left "FnCall could be not a constant."
evalConstExpr rt (FnParameter index)
  | Just ele <- getElem (arguments rt) index = evalConstExpr rt ele
  | otherwise = error $ "Could not find Parameter with index: " ++ show index

apply :: Runtime -> (Value -> Value -> ResultValue) -> Expr -> Expr -> ResultValue
apply rt f e1 e2 = do
  v1 <- evalConstExpr rt e1
  v2 <- evalConstExpr rt e2
  f v1 v2

evalAnd :: Runtime -> Expr -> Expr -> ResultValue
evalAnd rt e1 e2 = do
  v1 <- evalConstExpr rt e1
  b1 <- evalAsBool v1
  if b1
    then do
      v2 <- evalConstExpr rt e2
      b2 <- evalAsBool v2
      return $ VBool b2
    else
      return $ VBool False

evalOr :: Runtime -> Expr -> Expr -> ResultValue
evalOr rt e1 e2 = do
  v1 <- evalConstExpr rt e1
  b1 <- evalAsBool v1
  if b1
    then
      return $ VBool True
    else do
      v2 <- evalConstExpr rt e2
      b2 <- evalAsBool v2
      return $ VBool b2

evalNot :: Runtime -> Expr -> ResultValue
evalNot rt expr = do
  value <- evalConstExpr rt expr
  bool <- evalAsBool value
  return $ VBool $ not bool

evalIfThenElse :: Runtime -> Expr -> Expr -> Expr -> ResultValue
evalIfThenElse rt e1 e2 e3 = do
  v1 <- evalConstExpr rt e1
  bool <- evalAsBool v1
  if bool
    then
      evalConstExpr rt e2
    else
      evalConstExpr rt e3

evalCreateTuple :: Runtime -> Expr -> Expr -> ResultValue
evalCreateTuple rt e1 e2 = do
  v1 <- evalConstExpr rt e1
  v2 <- evalConstExpr rt e2
  return $ VTuple v1 v2

evalAsBool :: Value -> Either ErrorString Bool
evalAsBool (VBool b) = Right b
evalAsBool v1 = Left $ "Error: Expected Float got " <> show v1 <> "."

evalAsFloat :: Value -> Either ErrorString Double
evalAsFloat (VFloat f) = Right f
evalAsFloat v1 = Left $ "Error: Expected Float got " <> show v1 <> "."

evalArithmetic :: (Double -> Double -> Double) -> Value -> Value -> ResultValue
evalArithmetic f (VFloat x) (VFloat y) = Right (VFloat $ f x y)
evalArithmetic _ v1 v2 = Left $ "Error: Can't calculate a " <> show v1 <> " with " <> show v2 <> "."

evalCompare :: (forall a. (Eq a, Ord a) => a -> a -> Bool) -> Value -> Value -> ResultValue
evalCompare f (VFloat x) (VFloat y) = Right (VBool $ f x y)
evalCompare f (VBool x) (VBool y) = Right (VBool $ f x y)
evalCompare _ v1 v2 = Left $ "Error: Can't compare " <> show v1 <> " with " <> show v2 <> "."