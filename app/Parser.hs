module Parser
  ( Expr (..),
    Value (..),
    parseText,
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

-- shift reduce parser
init :: [String] -> Expr
init input = srp ([], input)

type Stack = [String]

type Input = [String]

srp :: (Stack, Input) -> Expr
srp (stack, input) = Const (VBool True)