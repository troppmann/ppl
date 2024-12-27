module Parser.String
  ( Func,
    ErrorString,
    Symbol,
    VariableName,
    separate,
    escape,
    toString,
    isViableName,
    isKeyword,
  )
where
import Representation
import Data.Char

separate :: String -> [String]
separate = words . escape . handleWhiteSpace OneLine


data WhiteSpace = OneLine | Stick | Separate | LineComment WhiteSpace | BlockComment WhiteSpace deriving (Show, Eq)
handleWhiteSpace :: WhiteSpace -> String -> String
handleWhiteSpace _ [] = ""
handleWhiteSpace previous ('/': '*' : xs) = handleWhiteSpace (BlockComment previous) xs
handleWhiteSpace (BlockComment previous) ('*': '/' : xs) = handleWhiteSpace previous xs
handleWhiteSpace (BlockComment previous) (_ : xs) = handleWhiteSpace (BlockComment previous) xs
handleWhiteSpace previous ('/': '/' : xs) = handleWhiteSpace (LineComment previous) xs
handleWhiteSpace OneLine (x: xs)
  | x == '\n' = handleWhiteSpace Stick xs
  | otherwise = x : handleWhiteSpace OneLine xs
handleWhiteSpace Stick (x: xs)
  | x == '\n' = handleWhiteSpace Separate xs
  | isSpace x = handleWhiteSpace Stick xs
  | otherwise = ' ' : x : handleWhiteSpace OneLine xs
handleWhiteSpace Separate (x: xs)
  | isSpace x = handleWhiteSpace Separate xs
  | otherwise = ';' : x : handleWhiteSpace OneLine xs
handleWhiteSpace (LineComment previous) ('\n': xs)
  | previous == OneLine = handleWhiteSpace Stick xs
  | previous == Stick = handleWhiteSpace Separate xs
  | previous == Separate = handleWhiteSpace Separate xs
handleWhiteSpace (LineComment previous) (_: xs) = handleWhiteSpace (LineComment previous) xs

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
escape ('=' :  xs) = " = " ++ escape xs
escape ('!' : '=' : xs) = " != " ++ escape xs
escape ('!' : xs) = " ! " ++ escape xs
escape ('<' : '=' : xs) = " <= " ++ escape xs
escape ('<' : xs) = " < " ++ escape xs
escape ('>' : '=' : xs) = " >= " ++ escape xs
escape ('>' : xs) = " > " ++ escape xs
escape (';' : xs) = " ; " ++ escape xs
escape (x : xs) = x : escape xs
escape "" = ""

type Func = String

type ErrorString = String

type Symbol = String
type VariableName = String

isViableName :: String -> Bool
isViableName [] = False
isViableName x
  | isKeyword x = False
isViableName (x:xs)
  | isLetter x && all isAlphaNum xs = True
  | otherwise = False

isKeyword :: String -> Bool
isKeyword "if" = True
isKeyword "else" = True
isKeyword "then" = True
isKeyword "True" = True
isKeyword "False" = True
isKeyword "Uniform" = True
isKeyword "Normal" = True
isKeyword _ = False

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
toString (FnCall fnName expr) = do
  let args = unwords $ map toString expr
  "(" ++ fnName ++ " " ++ args ++ ")"
toString (FnParameter index) = "Arg" ++ show index


middle :: String -> Expr -> Expr -> String
middle s e1 e2 = "(" ++ toString e1 ++ " " ++  s ++ " " ++ toString e2 ++ ")"