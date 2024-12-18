module Parser.String
  ( Func,
    ErrorString,
    Symbol,
    separate,
    escape,
    toString,
  )
where
import Representation

separate :: String -> [String]
separate = words . escape

escape :: String -> String
escape ('_' : xs) = " _ " ++ escape xs
escape ('(' : xs) = " ( " ++ escape xs
escape (')' : xs) = " ) " ++ escape xs
escape (',' : xs) = " , " ++ escape xs
escape ('+' : xs) = " + " ++ escape xs
escape ('-' : xs) = " - " ++ escape xs
escape ('*': '*' : xs) = " ** " ++ escape xs
escape ('*' : xs) = " * " ++ escape xs
escape ('/' : xs) = " / " ++ escape xs
escape ('=' : '=' : xs) = " == " ++ escape xs
escape ('!' : '=' : xs) = " != " ++ escape xs
escape ('!' : xs) = " ! " ++ escape xs
escape ('<' : '=' : xs) = " <= " ++ escape xs
escape ('<' : xs) = " < " ++ escape xs
escape ('>' : '=' : xs) = " >= " ++ escape xs
escape ('>' : xs) = " > " ++ escape xs
escape (x : xs) = x : escape xs
escape "" = ""

type Func = String

type ErrorString = String

type Symbol = String

toString :: Expr -> String
toString Normal = "Normal"
toString Uniform = "Uniform"
toString (Const (VFloat x)) = show x
toString (Const (VBool x)) = show x
toString (Const _) = error "No tuple"
toString (Plus e1 e2) = middle "+" e1 e2
toString (Multiply e1 e2) =  middle "*" e1 e2
toString (Subtract e1 e2) =  middle "-" e1 e2
toString (Divide e1 e2) =  middle "/" e1 e2
toString (And e1 e2) =  middle "&&" e1 e2
toString (Or e1 e2) = middle "||" e1 e2
toString (Not expr) = "!" ++ toString expr
toString (Equal e1 e2) = middle "==" e1 e2
toString (Unequal e1 e2) = middle "!=" e1 e2
toString (LessThan e1 e2) = middle "<" e1 e2
toString (LessThanOrEqual e1 e2) = middle "<=" e1 e2
toString (GreaterThan e1 e2) = middle ">" e1 e2
toString (GreaterThanOrEqual e1 e2) = middle ">=" e1 e2
toString (IfThenElse e1 e2 e3) = "if " ++ toString e1 ++ " then " ++ toString e2 ++ " else " ++ toString e3 
toString (CreateTuple e1 e2) = "(" ++ toString e1 ++ ", " ++ toString e2 ++ ")"
toString (Exponent e1 e2) = middle "**" e1 e2


middle :: String -> Expr -> Expr -> String
middle s e1 e2 = "(" ++ toString e1 ++ " " ++  s ++ " " ++ toString e2 ++ ")"