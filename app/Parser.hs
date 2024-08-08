module Parser
  ( parseText,
  )
where

import Representation
import Text.Read (readMaybe)

parseText :: String -> Expr
parseText = fst . parseUntil Nothing Nothing Nothing . separate

separate :: String -> [String]
separate = words . concatMap escape

escape :: Char -> String
escape '(' = " ( "
escape ')' = " ) "
escape c = [c]

-- parseUntil approach
type Func = String

type Symbol = String

parseUntil :: Maybe Expr -> Maybe Func -> Maybe Symbol -> [Symbol] -> (Expr, [Symbol])
parseUntil (Just e) Nothing (Just s) (x : xs)
  | s == x = (e, xs)
parseUntil Nothing _ _ [] = error "Error: Expected Value got ''"
parseUntil Nothing Nothing s (x : xs)
  | isFunction x = error "Error: Expected Value got Operator '*'"
  | x == "(" = do
      let (e, rest) = parseUntil Nothing Nothing (Just ")") xs
      parseUntil (Just e) Nothing s rest
  | x == "if" = do
      let (boolExpression, rest1) = parseUntil Nothing Nothing (Just "then") xs
      let (eIf1, rest2) = parseUntil Nothing Nothing (Just "else") rest1
      let (eIf2, rest3) = parseUntil Nothing Nothing s rest2
      let e = IfElseThen boolExpression eIf1 eIf2
      parseUntil (Just e) Nothing s rest3
  | e@(Just _) <- tryConvertToLiteral x = parseUntil e Nothing s xs
  | otherwise = error ("Error: Unknown String '" <> x <> "'")
parseUntil (Just e) Nothing s (x : xs)
  | isFunction x = parseUntil (Just e) (Just x) s xs
  | otherwise = error ("Error: Unexpected String'" <> x <> "'")
parseUntil (Just _e) (Just _f) _ [] = error "Expected Value got ''"
parseUntil Nothing (Just _f) _ (_x : _xs) = error "Cannot Happen yet"
parseUntil (Just e1) (Just f) s (x : xs)
  | isFunction x = error "Error: Expected Value got Operator '*'"
  | Just e2 <- tryConvertToLiteral x = parseUntil (Just $ combineFunction e1 f e2) Nothing s xs
  | x == "(" = do
      let (e2, rest) = parseUntil Nothing Nothing (Just ")") xs
      parseUntil (Just $ combineFunction e1 f e2) Nothing s rest
  | x == "if" = do
      let (boolExpression, rest1) = parseUntil Nothing Nothing (Just "then") xs
      let (eIf1, rest2) = parseUntil Nothing Nothing (Just "else") rest1
      let (eIf2, rest3) = parseUntil Nothing Nothing s rest2
      let e2 = IfElseThen boolExpression eIf1 eIf2
      parseUntil (Just $ combineFunction e1 f e2) Nothing s rest3
  | otherwise = error ("Error: Unexpected String'" <> x <> "'")
parseUntil (Just e) Nothing _ [] = (e, [])

isFunction :: String -> Bool
isFunction = flip elem ["+", "-", "*", "==", "!=", "<", "<=", ">", ">=", "&&", "||"]

combineFunction :: Expr -> Symbol -> Expr -> Expr
combineFunction e1 "+" e2 = Plus e1 e2
combineFunction e1 "*" e2 = Multiply e1 e2
combineFunction e1 "-" e2 = Subtract e1 e2
combineFunction e1 "==" e2 = Equal e1 e2
combineFunction e1 "!=" e2 = Unequal e1 e2
combineFunction e1 "<" e2 = LessThan e1 e2
combineFunction e1 "<=" e2 = LessEqualThan e1 e2
combineFunction e1 ">" e2 = GreaterThan e1 e2
combineFunction e1 ">=" e2 = GreaterEqualThan e1 e2
combineFunction e1 "&&" e2 = And e1 e2
combineFunction e1 "||" e2 = Or e1 e2
combineFunction _ _ _ = error "Unknown Function"

tryConvertToLiteral :: Symbol -> Maybe Expr
tryConvertToLiteral "True" = Just $ Const $ VBool True
tryConvertToLiteral "False" = Just $ Const $ VBool False
tryConvertToLiteral "Normal" = Just Normal
tryConvertToLiteral "Uniform" = Just Uniform
tryConvertToLiteral x
  | Just float <- readMaybe x :: Maybe Float = Just $ Const $ VFloat float
  | otherwise = Nothing