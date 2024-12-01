module Parser.Expr
  ( parseExpr,
  )
where

import Parser.String
import Representation
import Text.Read (readMaybe)

parseExpr :: String -> Either ErrorString Expr
parseExpr = fmap fst . parseUntil Nothing Nothing (Nothing, Nothing) . separate

parseUntil :: Maybe Expr -> Maybe Func -> (Maybe Symbol, Maybe Symbol) -> [Symbol] -> Either ErrorString (Expr, [Symbol])
parseUntil (Just e) Nothing (Just "else", Just s) (x : xs)
  | s == x || x == "," = return (e, x : xs)
parseUntil (Just e) Nothing (Just opening, Just ")") ("," : xs)
  | opening `elem` ["(", ","] = do
      (expr, rest) <- parseUntil Nothing Nothing (Just ",", Just ")") xs
      return (CreateTuple e expr, rest)
parseUntil (Just e) Nothing (_, Just s) (x : xs)
  | s == x = return (e, xs)
parseUntil Nothing _ _ [] = Left "Error: Expected Value got ''"
parseUntil Nothing Nothing s@(_, c) (x : xs)
  | isInfixFunction x = Left $ "Error: Expected Value got Operator '" <> x <> "'"
  | isFunction x = do
      (expr, rest) <- parseFirstExpression xs
      func <- applyFunction x expr
      parseUntil (Just func) Nothing s rest
  | x == "(" = do
      (e, rest) <- parseUntil Nothing Nothing (Just "(", Just ")") xs
      parseUntil (Just e) Nothing s rest
  | x == "if" = do
      (boolExpression, rest1) <- parseUntil Nothing Nothing (Just "if", Just "then") xs
      (eIf1, rest2) <- parseUntil Nothing Nothing (Just "then", Just "else") rest1
      (eIf2, rest3) <- parseUntil Nothing Nothing (Just "else", c) rest2
      let ifExpr = IfThenElse boolExpression eIf1 eIf2
      parseUntil (Just ifExpr) Nothing s rest3
  | e@(Just _) <- tryConvertToLiteral x = parseUntil e Nothing s xs
  | otherwise = Left $ "Error: Unknown String '" <> x <> "'"
parseUntil (Just e) Nothing s (x : xs)
  | isInfixFunction x = parseUntil (Just e) (Just x) s xs
  | otherwise = Left $ "Error: Unexpected String'" <> x <> "'"
parseUntil (Just _e) (Just _f) _ [] = Left "Expected Value got ''"
parseUntil Nothing (Just _f) _ (_x : _xs) = Left "Cannot happen yet"
parseUntil (Just e1) (Just f) s@(_, c) (x : xs)
  | isInfixFunction x = Left $ "Error: Expected Value got Operator '" <> x <> "'"
  | isFunction x = do
      (e2, rest) <- parseFirstExpression xs
      func <- applyFunction x e2
      combinedFunc <- combineFunction e1 f func
      parseUntil (Just combinedFunc) Nothing s rest
  | Just e2 <- tryConvertToLiteral x = do
      combinedFunc <- combineFunction e1 f e2
      parseUntil (Just combinedFunc) Nothing s xs
  | x == "(" = do
      (e2, rest) <- parseUntil Nothing Nothing (Just "(", Just ")") xs
      combinedFunc <- combineFunction e1 f e2
      parseUntil (Just combinedFunc) Nothing s rest
  | x == "if" = do
      (boolExpression, rest1) <- parseUntil Nothing Nothing (Just "if", Just "then") xs
      (eIf1, rest2) <- parseUntil Nothing Nothing (Just "then", Just "else") rest1
      (eIf2, rest3) <- parseUntil Nothing Nothing (Just "else", c) rest2
      let e2 = IfThenElse boolExpression eIf1 eIf2
      combinedIfExpr <- combineFunction e1 f e2
      parseUntil (Just combinedIfExpr) Nothing s rest3
  | otherwise = Left $ "Error: Unexpected String'" <> x <> "'"
parseUntil (Just e) Nothing _ [] = return (e, [])

parseFirstExpression :: [Symbol] -> Either ErrorString (Expr, [Symbol])
parseFirstExpression [] = error "Error: Expected Value got ''"
parseFirstExpression (x : xs)
  | isInfixFunction x = Left $ "Error: Expected Value got Operator '" <> x <> "'"
  | isFunction x = Left $ "Error: Expected Value got Operator '" <> x <> "'"
  | Just expr <- tryConvertToLiteral x = return (expr, xs)
  | x == "(" = parseUntil Nothing Nothing (Just "(", Just ")") xs
  | otherwise = Left ("Error: Unexpected String'" <> x <> "'")

isInfixFunction :: Symbol -> Bool
isInfixFunction = flip elem ["+", "-", "*", "/", "==", "!=", "<", "<=", ">", ">=", "&&", "||"]

isFunction :: Symbol -> Bool
isFunction = flip elem ["!"]

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
combineFunction _ f _ = Left $ "Unknown Function '" <> f <> "'"

applyFunction :: Symbol -> Expr -> Either ErrorString Expr
applyFunction "!" expr = return $ Not expr
applyFunction f _ = Left $ "Unknown Function '" <> f <> "'"

tryConvertToLiteral :: Symbol -> Maybe Expr
tryConvertToLiteral "True" = Just $ Const $ VBool True
tryConvertToLiteral "False" = Just $ Const $ VBool False
tryConvertToLiteral "Normal" = Just Normal
tryConvertToLiteral "Uniform" = Just Uniform
tryConvertToLiteral x
  | Just float <- readMaybe x :: Maybe Double = Just $ Const $ VFloat float
  | otherwise = Nothing