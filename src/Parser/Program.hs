module Parser.Program
  ( parseProgram,
  )
where

import Representation
import Parser.String
import Parser.Expr

parseProgram :: String -> Either ErrorString Program
parseProgram = fmap fst . parseProgramUntil [] Nothing . separate

parseProgramUntil :: [VariableName] -> Maybe Func -> [Symbol] -> Either ErrorString (Program, [Symbol])
parseProgramUntil _ Nothing [] = return ([],[])
parseProgramUntil _ (Just fnName) [] = Left $ "Function \"" ++ fnName ++ "\" has no body."
parseProgramUntil vs Nothing (x:xs)
  | isViableName x = parseProgramUntil vs (Just x) xs
  | otherwise = Left "fnName should be valid."
parseProgramUntil vs (Just fnName) ("=":xs) = do
  (expr, r1) <- parseUntil vs Nothing Nothing (Nothing, Just ";") xs
  (program, r2) <- parseProgramUntil [] Nothing r1
  return ((fnName, expr) : program, r2)
parseProgramUntil vs (Just fnName) (x:xs)
  | isViableName x =  parseProgramUntil (vs ++ [x]) (Just fnName) xs
  | otherwise = Left "Variable names should be valid."

