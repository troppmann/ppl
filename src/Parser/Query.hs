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
parseUntil (Just q) Nothing (Just ")") (")" : xs) = return (q, xs)
parseUntil Nothing Nothing s (x : xs)
  | x == "(" = do
      (q, rest) <- parseUntil Nothing Nothing (Just ")") xs
      parseUntil (Just q) Nothing s rest
  | x == "_" = parseUntil (Just QAny) Nothing s xs
  | x == "True" = parseUntil (Just $ QBool NormalMode True) Nothing s xs
  | x == "False" = parseUntil (Just $ QBool NormalMode False) Nothing s xs
  | Just float <- readMaybe x :: Maybe Double = parseUntil (Just $ QFloat NormalMode float) Nothing s xs
parseUntil q@(Just QAny) Nothing s (x : xs)
  | isInfixComp x = parseUntil q (Just x) s xs
parseUntil q@(Just (QFloat NormalMode _)) Nothing s (x : xs)
  | isInfixComp x = parseUntil q (Just x) s xs
parseUntil (Just (QFloat NormalMode float)) (Just func) s ("_" : xs)
  | func == "<" = parseUntil (Just $ QGt NormalMode float) Nothing s xs
  | func == "<=" = parseUntil (Just $ QGe NormalMode float) Nothing s xs
  | func == ">" = parseUntil (Just $ QLt NormalMode float) Nothing s xs
  | func == ">=" = parseUntil (Just $ QLe NormalMode float) Nothing s xs
parseUntil (Just QAny) (Just func) s (x : xs)
  | Just float <- readMaybe x :: Maybe Double = do
      query <- toQueryWithAny func float
      parseUntil (Just query) Nothing s xs
parseUntil (Just q) Nothing _ [] = return (q, [])
parseUntil q f s xs = todo $ "Case not handled: " <> show q <> " " <> show f <> " " <> show s <> " " <> show xs

isInfixComp :: Symbol -> Bool
isInfixComp = flip elem ["<", "<=", ">", ">="]

toQueryWithAny :: Func -> Double -> Either ErrorString QueryType
toQueryWithAny "<" f = return $ QLt NormalMode f
toQueryWithAny "<=" f = return $ QLe NormalMode f
toQueryWithAny ">" f = return $ QGt NormalMode f
toQueryWithAny ">=" f = return $ QGe NormalMode f
toQueryWithAny _ _ = Left "Cant convert this func to comparison."