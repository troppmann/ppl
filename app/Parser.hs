module Parser
  ( parseText,
  )
where

import Representation
import Text.Read (readMaybe)

parseText :: String -> Expr
parseText = parse . separate

separate :: String -> [String]
separate = words

-- TODO Precedence + * or parenthesis
parse :: [String] -> Expr
parse [] = error "Error: Expected Expr got ''"
parse ["True"] = Const (VBool True)
parse ["False"] = Const (VBool False)
parse ["Uniform"] = Uniform
parse ["Normal"] = Normal
parse (x : "*" : xs) = Multiply (parse [x]) (parse xs)
parse (x : "+" : xs) = Plus (parse [x]) (parse xs)
parse (x : "-" : xs) = Subtract (parse [x]) (parse xs)
parse (x : xs)
  | xs /= [] = error ("Error: Expected no input got '" <> show xs <> "'")
  | Just float <- readMaybe x :: Maybe Float = Const (VFloat float)
  | otherwise = error ("Error: Unknown input '" <> x <> "'")
