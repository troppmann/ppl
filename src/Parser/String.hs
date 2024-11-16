module Parser.String
  ( Func,
    ErrorString,
    Symbol,
    separate,
    escape,
  )
where

separate :: String -> [String]
separate = words . escape

escape :: String -> String
escape ('(' : xs) = " ( " ++ escape xs
escape (')' : xs) = " ) " ++ escape xs
escape (',' : xs) = " , " ++ escape xs
escape ('!' : '=' : xs) = " != " ++ escape xs
escape ('!' : xs) = " ! " ++ escape xs
escape (x : xs) = x : escape xs
escape "" = ""

type Func = String

type ErrorString = String

type Symbol = String
