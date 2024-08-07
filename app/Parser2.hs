module Parser2
  ( parseText,
    tokenizer,
    rpn,
    Token (..),
  )
where

import Text.Read

parseText :: String -> [Token]
parseText = tokenizer . separate

separate :: String -> [String]
separate = words

data Token = TLiteral Float | TPlus | TMultiply | TSubtract deriving (Show, Read, Eq)

tokenizer :: [String] -> [Token]
tokenizer [] = []
tokenizer ("+" : xs) = TPlus : tokenizer xs
tokenizer ("*" : xs) = TMultiply : tokenizer xs
tokenizer ("-" : xs) = TSubtract : tokenizer xs
tokenizer (x : xs)
  | Just float <- readMaybe x :: Maybe Float = TLiteral float : tokenizer xs
  | otherwise = error ("Unknown Symbol '" <> show x <> "'")

precedence :: Token -> Int
precedence TSubtract = 2
precedence TPlus = 2
precedence TMultiply = 3
precedence _ = error "No Precedence"

-- Shunting Yard Algorithm
-- HoldingStack(init empty) -> Input -> Output
rpn :: [Token] -> [Token] -> [Token]
rpn [] [] = []
rpn hs (x@(TLiteral _) : xs) = x : rpn hs xs
rpn [] (x : xs) = rpn [x] xs
rpn (h : hs) [] = h : rpn hs []
rpn (h : hs) (x : xs)
  | precedence x > precedence h = rpn (x : h : hs) xs
  | otherwise = h : rpn hs (x : xs)