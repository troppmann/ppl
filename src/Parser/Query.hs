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
parseUntil (Just q) Nothing s ("," : xs) = do
  (nextQuery, rest) <- parseUntil Nothing Nothing s xs
  parseUntil (Just $ QTuple q nextQuery) Nothing s rest
parseUntil (Just q) Nothing (Just ")") (")" : xs) = return $ dbg (q, xs)
parseUntil Nothing Nothing s (x : xs)
  | x == "(" = do
      (q, rest) <- parseUntil Nothing Nothing (Just ")") xs
      parseUntil (Just q) Nothing s (dbg rest)
  | x == "_" = parseUntil (Just QAny) Nothing s xs
  | x == "True" = parseUntil (Just $ QIs True) Nothing s xs
  | x == "False" = parseUntil (Just $ QIs False) Nothing s xs
  | Just float <- readMaybe x :: Maybe Double = parseUntil (Just $ QAt float) Nothing s xs
parseUntil q@(Just QAny) Nothing s (x : xs)
  | isInfixComp x = parseUntil q (Just x) s xs
parseUntil q@(Just (QAt _)) Nothing s (x : xs)
  | isInfixComp x = parseUntil q (Just x) s xs
parseUntil (Just (QAt float)) (Just func) s ("_" : xs)
  | func == "<" = parseUntil (Just $ QGt float) Nothing s xs
  | func == "<=" = parseUntil (Just $ QGe float) Nothing s xs
  | func == ">" = parseUntil (Just $ QLt float) Nothing s xs
  | func == ">=" = parseUntil (Just $ QLe float) Nothing s xs
parseUntil (Just QAny) (Just func) s (x : xs)
  | Just float <- readMaybe x :: Maybe Double = do
      query <- toQueryWithAny func float
      parseUntil (Just query) Nothing s xs
parseUntil (Just q) Nothing _ [] = return (q, [])
parseUntil q f s xs = todo $ "Case not handled: " <> show q <> " " <> show f <> " " <> show s <> " " <> show xs

isInfixComp :: Symbol -> Bool
isInfixComp = flip elem ["<", "<=", ">", ">="]

toQueryWithAny :: Func -> Double -> Either ErrorString QueryType
toQueryWithAny "<" f = return $ QLt f
toQueryWithAny "<=" f = return $ QLe f
toQueryWithAny ">" f = return $ QGt f
toQueryWithAny ">=" f = return $ QGe f
toQueryWithAny _ _ = Left "Cant convert this func to comparison."