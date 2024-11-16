module Parser.Query
  ( parseQuery,
  )
where

import Debug.Extended
import Parser.String
import Query
import Representation
import Text.Read (readMaybe)

parseQuery :: String -> Either ErrorString QueryType
parseQuery = fmap fst . parseUntil Nothing Nothing (Nothing, Nothing) . separate

parseUntil :: Maybe QueryType -> Maybe Func -> (Maybe Symbol, Maybe Symbol) -> [Symbol] -> Either ErrorString (QueryType, [Symbol])
parseUntil Nothing Nothing s (x : xs)
  | q@(Just _) <- tryConvertToLiteral x = parseUntil q Nothing s xs
parseUntil (Just query) Nothing (Nothing, Nothing) [] = return (query, [])

tryConvertToLiteral :: Symbol -> Maybe QueryType
tryConvertToLiteral "_" = Just QAny
tryConvertToLiteral "True" = Just $ QIs True
tryConvertToLiteral "False" = Just $ QIs False
tryConvertToLiteral x
  | Just float <- readMaybe x :: Maybe Double = Just $ QAt float
  | otherwise = Nothing