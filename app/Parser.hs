module Parser
  ( parseText,
  )
where

import Representation
import Text.Read (readMaybe)

parseText :: String -> Expr
parseText = parse Nothing . separate

separate :: String -> [String]
separate = words

-- TODO Precedence + * or parenthesis
parse :: Maybe Expr -> [String] -> Expr
parse Nothing [] = error "Error: Expected Expr got ''"
parse (Just expr) [] = expr
parse Nothing ("True" : xs) = parse (Just $ Const $ VBool True) xs
parse Nothing ("False" : xs) = parse (Just $ Const $ VBool False) xs
parse Nothing ("Uniform" : xs) = parse (Just Uniform) xs
parse Nothing ("Normal" : xs) = parse (Just Normal) xs
parse (Just _) ("True" : _) = error "Error: Expected Operator got 'True'"
parse (Just _) ("False" : _) = error "Error: Expected Operator got 'False'"
parse (Just _) ("Uniform" : _) = error "Error: Expected Operator got 'Uniform'"
parse (Just _) ("Normal" : _) = error "Error: Expected Operator got 'Normal'"
parse (Just expr) ("*" : xs) = Multiply expr (parse Nothing xs)
parse (Just expr) ("+" : xs) = Plus expr (parse Nothing xs)
parse (Just expr) ("-" : xs) = Subtract expr (parse Nothing xs)
parse Nothing ("*" : _) = error "Error: Expected Value got Operator '*'"
parse Nothing ("+" : _) = error "Error: Expected Value got Operator '+'"
parse Nothing ("-" : _) = error "Error: Expected Value got Operator '-'"
parse Nothing (unknownString : xs)
  | Just float <- readMaybe unknownString :: Maybe Float = parse (Just $ Const $ VFloat float) xs
  | otherwise = error ("Error: Unknown String '" <> unknownString <> "'")
parse (Just _) (unknownString : _) = error ("Error: Expected Operator got '" <> unknownString <> "'")