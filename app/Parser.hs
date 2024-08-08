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

-- TODO left to right instead of right to left
parse :: Maybe Expr -> Maybe ControlFlow -> [String] -> (Expr, [String])
parse Nothing Nothing [] = error "Error: Expected Expr got ''"
parse (Just expr) Nothing [] = (expr, [])
parse _ (Just OpenParenthesis) [] = error "error: expected ) got ''"
parse (Just expr) (Just OpenParenthesis) (")" : xs) = (expr, xs)
parse Nothing c ("(" : xs) = parse (Just expr) c rest
  where
    (expr, rest) = parse Nothing (Just OpenParenthesis) xs
parse Nothing c ("True" : xs) = parse (Just $ Const $ VBool True) c xs
parse Nothing c ("False" : xs) = parse (Just $ Const $ VBool False) c xs
parse Nothing c ("Uniform" : xs) = parse (Just Uniform) c xs
parse Nothing c ("Normal" : xs) = parse (Just Normal) c xs
parse (Just _) _ ("True" : _) = error "Error: Expected Operator got 'True'"
parse (Just _) _ ("False" : _) = error "Error: Expected Operator got 'False'"
parse (Just _) _ ("Uniform" : _) = error "Error: Expected Operator got 'Uniform'"
parse (Just _) _ ("Normal" : _) = error "Error: Expected Operator got 'Normal'"
parse (Just fExpr) c ("*" : xs) = (Multiply fExpr sExpr, rest)
  where
    (sExpr, rest) = parse Nothing c xs
parse (Just fExpr) c ("+" : xs) = (Plus fExpr sExpr, rest)
  where
    (sExpr, rest) = parse Nothing c xs
parse (Just fExpr) c ("-" : xs) = (Subtract fExpr sExpr, rest)
  where
    (sExpr, rest) = parse Nothing c xs
parse Nothing _ ("*" : _) = error "Error: Expected Value got Operator '*'"
parse Nothing _ ("+" : _) = error "Error: Expected Value got Operator '+'"
parse Nothing _ ("-" : _) = error "Error: Expected Value got Operator '-'"
parse Nothing c (unknownString : xs)
  | Just float <- readMaybe unknownString :: Maybe Float = parse (Just $ Const $ VFloat float) c xs
  | otherwise = error ("Error: Unknown String '" <> unknownString <> "'")
parse (Just _) _ (unknownString : _) = error ("Error: Expected Operator got '" <> unknownString <> "'")

-- big stack approach
data Function = Infix String

p2 :: Maybe Expr -> Maybe ControlFlow -> Maybe Function -> [String] -> Expr
p2 Nothing _ _ [] = error "Error: Expected Expr got ''"
p2 Nothing Nothing Nothing (x : xs)
  | x `elem` ["+", "-", "*"] = error "Error: Expected Value got Operator '*'"
  | Just float <- readMaybe x :: Maybe Float = p2 (Just $ Const $ VFloat float) Nothing Nothing xs
  | otherwise = error ("Error: Unknown String '" <> x <> "'")
p2 (Just e) Nothing Nothing (x : xs)
  | x `elem` ["+", "-", "*"] = p2 (Just e) Nothing (Just $ Infix x) xs
  | otherwise = error ("Error: Unexpected String'" <> x <> "'")
p2 (Just e) Nothing (Just (Infix f)) (x : xs)
  | x `elem` ["+", "-", "*"] = error "Error: Expected Value got Operator '*'"
  | Just float <- readMaybe x :: Maybe Float = p2 (Just $ combineFunction e f (Const $ VFloat float)) Nothing Nothing xs
  | otherwise = error ("Error: Unexpected String'" <> x <> "'")
p2 (Just e) Nothing Nothing [] = e

-- early return approach
early :: [Expr] -> Maybe ControlFlow -> [String] -> (Expr, [String])
early _ _ [] = error "Nope"
early [] Nothing (x : xs)
  | x `elem` ["+", "-", "*"] = error "Error: Expected Value got Operator '*'"
  | Just float <- readMaybe x :: Maybe Float = (Const $ VFloat float, xs)
  | x == "(" = early [] (Just OpenParenthesis) xs
  | otherwise = error ("Error: Unknown String '" <> x <> "'")
early [] (Just OpenParenthesis) (x : xs)
  | x `elem` ["+", "-", "*"] = error "Error: Expected Value got Operator '*'"
  | Just float <- readMaybe x :: Maybe Float = early [Const $ VFloat float] (Just OpenParenthesis) xs
  | otherwise = error ("Error: Unknown String '" <> x <> "'")
early (e : _) Nothing (x : xs)
  | x `elem` ["+", "-", "*"] = (combineFunction e x fe, rest)
  | otherwise = error ("Error: Unknown String '" <> x <> "'")
  where
    (fe, rest) = early [] Nothing xs
early (e : _) (Just OpenParenthesis) (")" : xs) = (e, xs)
early (e : _) (Just OpenParenthesis) (x : xs)
  | x `elem` ["+", "-", "*"] = early [combineFunction e x fe] (Just OpenParenthesis) rest
  | otherwise = error ("Error: Unknown String '" <> x <> "'")
  where
    (fe, rest) = early [] Nothing xs

pEarly :: [Expr] -> [String] -> Expr
pEarly [] [] = error "Empty"
pEarly (e : _) [] = e
pEarly es xs = pEarly [fe] rest
  where
    (fe, rest) = early es Nothing xs

-- parseUntil approach
type Func = String

type Symbol = String

parseUntil :: Maybe Expr -> Maybe Func -> Maybe Symbol -> [Symbol] -> (Expr, [Symbol])
parseUntil (Just e) Nothing (Just s) (x : xs)
  | s == x = (e, xs)
parseUntil Nothing _ _ [] = error "Error: Expected Value got ''"
parseUntil Nothing Nothing s (x : xs)
  | isFunction x = error "Error: Expected Value got Operator '*'"
  | isOpenExpression x = parseUntil Nothing Nothing (Just $ untilSymbol x) xs
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
  | isOpenExpression x = do
      let (e2, rest) = parseUntil Nothing Nothing (Just $ untilSymbol x) xs
      parseUntil (Just $ combineFunction e1 f e2) Nothing s rest
  | otherwise = error ("Error: Unexpected String'" <> x <> "'")
parseUntil (Just e) Nothing _ [] = (e, [])

isFunction :: String -> Bool
isFunction = flip elem ["+", "-", "*"]

combineFunction :: Expr -> Symbol -> Expr -> Expr
combineFunction e1 "+" e2 = Plus e1 e2
combineFunction e1 "*" e2 = Multiply e1 e2
combineFunction e1 "-" e2 = Subtract e1 e2
combineFunction _ _ _ = error "Unknown Function"

isOpenExpression :: Symbol -> Bool
isOpenExpression "(" = True
isOpenExpression _ = False

untilSymbol :: Symbol -> Symbol
untilSymbol "(" = ")"
untilSymbol _ = error "Unknown until Symbol"

tryConvertToLiteral :: Symbol -> Maybe Expr
tryConvertToLiteral "True" = Just $ Const $ VBool True
tryConvertToLiteral "False" = Just $ Const $ VBool False
tryConvertToLiteral "Normal" = Just Normal
tryConvertToLiteral "Uniform" = Just Uniform
tryConvertToLiteral x
  | Just float <- readMaybe x :: Maybe Float = Just $ Const $ VFloat float
  | otherwise = Nothing