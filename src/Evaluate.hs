module Evaluate
  ( evalConstExpr,
    evalArithmetic,
    evalAsBool,
    evalAsFloat,
  )
where

-- TODO:  Sample and Evaluate ar mostly the same could be refactored together
--        but could be less readable
import Representation

type ErrorString = String

type ResultValue = Either ErrorString Value

evalConstExpr :: Expr -> ResultValue
evalConstExpr (Const v) = Right v
evalConstExpr Normal = Left "Normal is not a constant."
evalConstExpr Uniform = Left "Uniform is not a constant."
evalConstExpr (Plus e1 e2) = apply (evalArithmetic (+)) e1 e2
evalConstExpr (Multiply e1 e2) = apply (evalArithmetic (*)) e1 e2
evalConstExpr (Subtract e1 e2) = apply (evalArithmetic (-)) e1 e2
evalConstExpr (Divide e1 e2) = apply (evalArithmetic (/)) e1 e2
evalConstExpr (And e1 e2) = evalAnd e1 e2
evalConstExpr (Or e1 e2) = evalOr e1 e2
evalConstExpr (Not expr) = evalNot expr
evalConstExpr (Equal e1 e2) = apply (evalCompare (==)) e1 e2
evalConstExpr (Unequal e1 e2) = apply (evalCompare (/=)) e1 e2
evalConstExpr (LessThan e1 e2) = apply (evalCompare (<)) e1 e2
evalConstExpr (LessThanOrEqual e1 e2) = apply (evalCompare (<=)) e1 e2
evalConstExpr (GreaterThan e1 e2) = apply (evalCompare (>)) e1 e2
evalConstExpr (GreaterThanOrEqual e1 e2) = apply (evalCompare (>=)) e1 e2
evalConstExpr (IfElseThen e1 e2 e3) = evalIfElseThen e1 e2 e3

apply :: (Value -> Value -> ResultValue) -> Expr -> Expr -> ResultValue
apply f e1 e2 = do
  v1 <- evalConstExpr e1
  v2 <- evalConstExpr e2
  f v1 v2

evalAnd :: Expr -> Expr -> ResultValue
evalAnd e1 e2 = do
  v1 <- evalConstExpr e1
  b1 <- evalAsBool v1
  if b1
    then do
      v2 <- evalConstExpr e2
      b2 <- evalAsBool v2
      return $ VBool b2
    else
      return $ VBool False

evalOr :: Expr -> Expr -> ResultValue
evalOr e1 e2 = do
  v1 <- evalConstExpr e1
  b1 <- evalAsBool v1
  if b1
    then
      return $ VBool True
    else do
      v2 <- evalConstExpr e2
      b2 <- evalAsBool v2
      return $ VBool b2

evalNot :: Expr -> ResultValue
evalNot expr = do
  value <- evalConstExpr expr
  bool <- evalAsBool value
  return $ VBool $ not bool

evalIfElseThen :: Expr -> Expr -> Expr -> ResultValue
evalIfElseThen e1 e2 e3 = do
  v1 <- evalConstExpr e1
  bool <- evalAsBool v1
  if bool
    then
      evalConstExpr e2
    else
      evalConstExpr e3

evalAsBool :: Value -> Either ErrorString Bool
evalAsBool (VFloat _) = Left "Error: Expected Bool got Float."
evalAsBool (VBool b) = Right b

evalAsFloat :: Value -> Either ErrorString Double
evalAsFloat (VBool _) = Left "Error: Expected Float got Bool."
evalAsFloat (VFloat f) = Right f

evalArithmetic :: (Double -> Double -> Double) -> Value -> Value -> ResultValue
evalArithmetic _ (VBool _) _ = Left "Error: Can't calculate a Boolean Value."
evalArithmetic _ _ (VBool _) = Left "Error: Can't calculate a Boolean Value."
evalArithmetic f (VFloat x) (VFloat y) = Right (VFloat $ f x y)

evalCompare :: (forall a. (Eq a, Ord a) => a -> a -> Bool) -> Value -> Value -> ResultValue
evalCompare _ (VBool _) (VFloat _) = Left "Error: Can't compare Bool with Float."
evalCompare _ (VFloat _) (VBool _) = Left "Error: Can't compare Float with Bool."
evalCompare f (VFloat x) (VFloat y) = Right (VBool $ f x y)
evalCompare f (VBool x) (VBool y) = Right (VBool $ f x y)