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

parseUntil :: Maybe Expr -> Maybe Func -> (Maybe Symbol, Maybe Symbol) -> [Symbol] -> Either ErrorString (QueryType, [Symbol])
parseUntil = todo ""