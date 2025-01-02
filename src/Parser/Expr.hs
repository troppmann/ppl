module Parser.Expr
  ( parseExpr,
    parseUntil,
  )
where

import Data.List (elemIndex)
import Parser.String
import Representation
import Text.Read (readMaybe)

parseExpr :: String -> Either ErrorString Expr
parseExpr = fmap fst . parseUntil [] Nothing Nothing (Nothing, Nothing) . separate

parseUntil :: [VariableName] -> Maybe Expr -> Maybe Func -> (Maybe Symbol, Maybe Symbol) -> [Symbol] -> Either ErrorString (Expr, [Symbol])
parseUntil _ (Just e) Nothing (Just "else", Just s) (x : xs)
  | s == x || x == "," = return (e, x : xs)
parseUntil vs (Just e) Nothing (Just opening, Just ")") ("," : xs)
  | opening `elem` ["(", ","] = do
      (expr, rest) <- parseUntil vs Nothing Nothing (Just ",", Just ")") xs
      return (CreateTuple e expr, rest)
parseUntil _ (Just e) Nothing (_, Just s) (x : xs)
  | s == x = return (e, xs)
parseUntil _ Nothing _ _ [] = Left "Error: Expected Value got ''"
parseUntil vs Nothing Nothing s@(_, c) (x : xs)
  | x == "-" = do
      (e1, rest) <- parseFirstExpression vs xs
      let e2 =
            case e1 of
              (Const (VFloat v)) -> Const (VFloat (-v))
              _ -> Multiply (Const $ VFloat (-1)) e1
      parseUntil vs (Just e2) Nothing s rest
  | isInfixFunction x = Left $ "Error: Expected Value got Operator '" <> x <> "'"
  | isFunction x = do
      (expr, rest) <- parseFirstExpression vs xs
      func <- applyFunction x expr
      parseUntil vs (Just func) Nothing s rest
  | x == "(" = do
      (e, rest) <- parseUntil vs Nothing Nothing (Just "(", Just ")") xs
      parseUntil vs (Just e) Nothing s rest
  | x == "if" = do
      (boolExpression, rest1) <- parseUntil vs Nothing Nothing (Just "if", Just "then") xs
      (eThen, rest2) <- parseUntil vs Nothing Nothing (Just "then", Just "else") rest1
      (eElse, rest3) <- parseUntil vs Nothing Nothing (Just "else", c) rest2
      let ifExpr = IfThenElse boolExpression eThen eElse
      parseUntil vs (Just ifExpr) Nothing s rest3
  | e@(Just _) <- tryConvertToLiteral x = parseUntil vs e Nothing s xs
  | Just index <- x `elemIndex` vs = do
      let e1 = FnParameter index
      parseUntil vs (Just e1) Nothing s xs
  | otherwise = do
      (exprs, rest) <- parseExpressionsUntil vs c xs
      let e1 = FnCall x exprs
      parseUntil vs (Just e1) Nothing s rest
parseUntil vs (Just e) Nothing s (x : xs)
  | isInfixFunction x = parseUntil vs (Just e) (Just x) s xs
  | otherwise = Left $ "Error: Unexpected String'" <> x <> "'"
parseUntil _ (Just _e) (Just _f) _ [] = Left "Expected Value got ''"
parseUntil _ Nothing (Just _f) _ (_x : _xs) = Left "Cannot happen yet"
parseUntil vs (Just e1) (Just f) s@(_, c) (x : xs)
  | x == "-" = do
      (e2, rest) <- parseFirstExpression vs xs
      let e3 =
            case e2 of
              (Const (VFloat v)) -> Const (VFloat (-v))
              _ -> Multiply (Const $ VFloat (-1)) e2
      combinedFunc <- combineFunction e1 f e3
      parseUntil vs (Just combinedFunc) Nothing s rest
  | isInfixFunction x = Left $ "Error: Expected Value got Operator '" <> x <> "'"
  | isFunction x = do
      (e2, rest) <- parseFirstExpression vs xs
      func <- applyFunction x e2
      combinedFunc <- combineFunction e1 f func
      parseUntil vs (Just combinedFunc) Nothing s rest
  | Just e2 <- tryConvertToLiteral x = do
      combinedFunc <- combineFunction e1 f e2
      parseUntil vs (Just combinedFunc) Nothing s xs
  | x == "(" = do
      (e2, rest) <- parseUntil vs Nothing Nothing (Just "(", Just ")") xs
      combinedFunc <- combineFunction e1 f e2
      parseUntil vs (Just combinedFunc) Nothing s rest
  | x == "if" = do
      (boolExpression, rest1) <- parseUntil vs Nothing Nothing (Just "if", Just "then") xs
      (eIf1, rest2) <- parseUntil vs Nothing Nothing (Just "then", Just "else") rest1
      (eIf2, rest3) <- parseUntil vs Nothing Nothing (Just "else", c) rest2
      let e2 = IfThenElse boolExpression eIf1 eIf2
      combinedIfExpr <- combineFunction e1 f e2
      parseUntil vs (Just combinedIfExpr) Nothing s rest3
  | Just index <- x `elemIndex` vs = do
      let e2 = FnParameter index
      combinedFunc <- combineFunction e1 f e2
      parseUntil vs (Just combinedFunc) Nothing s xs
  | otherwise = do
      (exprs, rest) <- parseExpressionsUntil vs c xs
      let e2 = FnCall x exprs
      combinedFunc <- combineFunction e1 f e2
      parseUntil vs (Just combinedFunc) Nothing s rest
parseUntil _ (Just e) Nothing _ [] = return (e, [])

parseFirstExpression :: [VariableName] -> [Symbol] -> Either ErrorString (Expr, [Symbol])
parseFirstExpression _ [] = error "Error: Expected Value got ''"
parseFirstExpression vs (x : xs)
  | isInfixFunction x = Left $ "Error: Expected Value got Operator '" <> x <> "'"
  | isFunction x = Left $ "Error: Expected Value got Operator '" <> x <> "'"
  | Just expr <- tryConvertToLiteral x = return (expr, xs)
  | x == "(" = parseUntil vs Nothing Nothing (Just "(", Just ")") xs
  | Just index <- x `elemIndex` vs = do
      let e1 = FnParameter index
      return (e1, xs)
  | otherwise = Left ("Error: Unexpected String'" <> x <> "'")

parseExpressionsUntil :: [VariableName] -> Maybe Symbol -> [Symbol] -> Either ErrorString ([Expr], [Symbol])
parseExpressionsUntil _ _ [] = return ([], [])
parseExpressionsUntil _ (Just c) (x : xs)
  | x == "," = return ([], x : xs)
  | x == c = return ([], x : xs)
parseExpressionsUntil vs c (x : xs)
  | isInfixFunction x = Left $ "Error: Expected Value got Operator '" <> x <> "'"
  | isFunction x = Left $ "Error: Expected Value got Operator '" <> x <> "'"
  | Just expr <- tryConvertToLiteral x = do
      (exprs, rest) <- parseExpressionsUntil vs c xs
      return (expr : exprs, rest)
  | Just index <- x `elemIndex` vs = do
      let expr = FnParameter index
      (exprs, rest) <- parseExpressionsUntil vs c xs
      return (expr : exprs, rest)
  | x == "(" = do
      (expr, r1) <- parseUntil vs Nothing Nothing (Just "(", Just ")") xs
      (exprs, r2) <- parseExpressionsUntil vs c r1
      return (expr : exprs, r2)
  | otherwise = Left ("Error: Unexpected String'" <> x <> "'")

isInfixFunction :: Symbol -> Bool
isInfixFunction = flip elem ["+", "-", "*", "/", "==", "!=", "<", "<=", ">", ">=", "&&", "||", "**"]

isFunction :: Symbol -> Bool
isFunction = flip elem ["!", "abs"]

combineFunction :: Expr -> Symbol -> Expr -> Either ErrorString Expr
combineFunction e1 "+" e2 = return $ Plus e1 e2
combineFunction e1 "*" e2 = return $ Multiply e1 e2
combineFunction e1 "-" e2 = return $ Subtract e1 e2
combineFunction e1 "/" e2 = return $ Divide e1 e2
combineFunction e1 "==" e2 = return $ Equal e1 e2
combineFunction e1 "!=" e2 = return $ Unequal e1 e2
combineFunction e1 "<" e2 = return $ LessThan e1 e2
combineFunction e1 "<=" e2 = return $ LessThanOrEqual e1 e2
combineFunction e1 ">" e2 = return $ GreaterThan e1 e2
combineFunction e1 ">=" e2 = return $ GreaterThanOrEqual e1 e2
combineFunction e1 "&&" e2 = return $ And e1 e2
combineFunction e1 "||" e2 = return $ Or e1 e2
combineFunction e1 "**" e2 = return $ Exponent e1 e2
combineFunction _ f _ = Left $ "Unknown Function '" <> f <> "'"

applyFunction :: Symbol -> Expr -> Either ErrorString Expr
applyFunction "!" expr = return $ Not expr
applyFunction "abs" expr = return $ Abs expr
applyFunction f _ = Left $ "Unknown Function '" <> f <> "'"

tryConvertToLiteral :: Symbol -> Maybe Expr
tryConvertToLiteral "True" = Just $ Const $ VBool True
tryConvertToLiteral "False" = Just $ Const $ VBool False
tryConvertToLiteral "Normal" = Just Normal
tryConvertToLiteral "Uniform" = Just Uniform
tryConvertToLiteral x
  | Just float <- readMaybe x :: Maybe Double = Just $ Const $ VFloat float
  | otherwise = Nothing