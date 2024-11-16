module Parser.Query
  ( parseQuery,
  )
where

import Debug.Extended
import Parser.String
import Query
import Text.Read (readMaybe)

parseQuery :: String -> Either ErrorString QueryType
parseQuery = fmap fst . parseUntil Nothing Nothing . separate

parseUntil :: Maybe QueryType -> Maybe Func -> [Symbol] -> Either ErrorString (QueryType, [Symbol])
parseUntil Nothing Nothing (x : xs)
  | x == "_" = parseUntil (Just QAny) Nothing xs
  | x == "True" = parseUntil (Just $ QIs True) Nothing xs
  | x == "False" = parseUntil (Just $ QIs False) Nothing xs
  | Just float <- readMaybe x :: Maybe Double = parseUntil (Just $ QAt float) Nothing xs
parseUntil q@(Just QAny) Nothing (x : xs)
  | isInfixComp x = parseUntil q (Just x) xs
parseUntil q@(Just (QAt _)) Nothing (x : xs)
  | isInfixComp x = parseUntil q (Just x) xs
parseUntil (Just (QAt float)) (Just func) ("_" : xs)
  | func == "<" = parseUntil (Just $ QGt float) Nothing xs
  | func == "<=" = parseUntil (Just $ QGe float) Nothing xs
  | func == ">" = parseUntil (Just $ QLt float) Nothing xs
  | func == ">=" = parseUntil (Just $ QLe float) Nothing xs
parseUntil (Just QAny) (Just func) (x : xs)
  | Just float <- readMaybe x :: Maybe Double = do
      query <- toQueryWithAny func float
      parseUntil (Just query) Nothing xs
parseUntil (Just q) Nothing [] = return (q, [])
parseUntil _ _ _ = todo "Missing case"

isInfixComp :: Symbol -> Bool
isInfixComp = flip elem ["<", "<=", ">", ">="]

toQueryWithAny :: Func -> Double -> Either ErrorString QueryType
toQueryWithAny "<" f = return $ QLt f
toQueryWithAny "<=" f = return $ QLe f
toQueryWithAny ">" f = return $ QGt f
toQueryWithAny ">=" f = return $ QGe f
toQueryWithAny _ _ = Left "Cant convert this func to comparison."