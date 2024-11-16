module Parser.Query
  ( parseQuery,
  )
where

import Debug.Extended
import Parser.String
import Query
import Text.Read (readMaybe)

parseQuery :: String -> Either ErrorString QueryType
parseQuery = fmap fst . parseUntil Nothing Nothing Nothing . separate

parseUntil :: Maybe QueryType -> Maybe Func -> Maybe Symbol -> [Symbol] -> Either ErrorString (QueryType, [Symbol])
parseUntil (Just q) Nothing Nothing ("," : xs) = do
  (nextQuery, rest) <- parseUntil Nothing Nothing Nothing xs
  parseUntil (Just $ QTuple q nextQuery) Nothing Nothing rest
parseUntil Nothing Nothing Nothing (x : xs)
  | x == "_" = parseUntil (Just QAny) Nothing Nothing xs
  | x == "True" = parseUntil (Just $ QIs True) Nothing Nothing xs
  | x == "False" = parseUntil (Just $ QIs False) Nothing Nothing xs
  | Just float <- readMaybe x :: Maybe Double = parseUntil (Just $ QAt float) Nothing Nothing xs
parseUntil q@(Just QAny) Nothing Nothing (x : xs)
  | isInfixComp x = parseUntil q (Just x) Nothing xs
parseUntil q@(Just (QAt _)) Nothing Nothing (x : xs)
  | isInfixComp x = parseUntil q (Just x) Nothing xs
parseUntil (Just (QAt float)) (Just func) Nothing ("_" : xs)
  | func == "<" = parseUntil (Just $ QGt float) Nothing Nothing xs
  | func == "<=" = parseUntil (Just $ QGe float) Nothing Nothing xs
  | func == ">" = parseUntil (Just $ QLt float) Nothing Nothing xs
  | func == ">=" = parseUntil (Just $ QLe float) Nothing Nothing xs
parseUntil (Just QAny) (Just func) Nothing (x : xs)
  | Just float <- readMaybe x :: Maybe Double = do
      query <- toQueryWithAny func float
      parseUntil (Just query) Nothing Nothing xs
parseUntil (Just q) Nothing Nothing [] = return (q, [])
parseUntil _ _ _ _ = todo "Missing case"

isInfixComp :: Symbol -> Bool
isInfixComp = flip elem ["<", "<=", ">", ">="]

toQueryWithAny :: Func -> Double -> Either ErrorString QueryType
toQueryWithAny "<" f = return $ QLt f
toQueryWithAny "<=" f = return $ QLe f
toQueryWithAny ">" f = return $ QGt f
toQueryWithAny ">=" f = return $ QGe f
toQueryWithAny _ _ = Left "Cant convert this func to comparison."