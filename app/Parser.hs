module Parser
  ( Expr (..),
    Value (..),
    parseText,
    sample,
  )
where

import Text.Read (readMaybe)

data Expr
  = Const Value
  | Plus Expr Expr
  | Multiply Expr Expr
  | Subtract Expr Expr
  deriving (Show, Read, Eq)

data Value
  = VFloat Float
  | VBool Bool
  deriving (Show, Read, Eq)

parseText :: String -> Expr
parseText = parse . separate

separate :: String -> [String]
separate = words

-- TODO Precedence + * or parenthesis
parse :: [String] -> Expr
parse [] = error "Error: Expected Expr got ''"
parse ["True"] = Const (VBool True)
parse ["False"] = Const (VBool False)
parse (x : "*" : xs) = Multiply (parse [x]) (parse xs)
parse (x : "+" : xs) = Plus (parse [x]) (parse xs)
parse (x : "-" : xs) = Subtract (parse [x]) (parse xs)
parse (x : xs)
  | xs /= [] = error ("Error: Expected no input got '" <> show xs <> "'")
  | Just float <- readMaybe x :: Maybe Float = Const (VFloat float)
  | otherwise = error ("Error: Unknown input '" <> x <> "'")

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