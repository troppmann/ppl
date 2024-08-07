module Parser
  ( parseText,
  )
where

import Representation
import Text.Read (readMaybe)

parseText :: String -> Expr
parseText = fst . parse Nothing Nothing . separate

separate :: String -> [String]
separate = words . concatMap escape

escape :: Char -> String
escape '(' = " ( "
escape ')' = " ) "
escape '*' = " * "
escape '+' = " + "
escape '-' = " - "
escape c = [c]

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