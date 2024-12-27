module Parser.Program
  ( parseProgram,
    parseProgramWithOptions,
    ParseOptions(..),
  )
where

import Representation
import Parser.String
import Parser.Expr
import Optimizer qualified as Opti

parseProgram :: String -> Either ErrorString Program
parseProgram = parseProgramWithOptions ParseOptions{optimization=True,maxLoopUnroll=30}

data ParseOptions = ParseOptions{optimization::Bool, maxLoopUnroll::Int}
parseProgramWithOptions :: ParseOptions -> String -> Either ErrorString Program
parseProgramWithOptions options = fmap (optimizeStep . fst) . parseProgramUntil [] Nothing . separate
  where
    optimizeOption = Opti.OptimizeOption{Opti.maxLoopUnroll=maxLoopUnroll options}
    optimizeStep = if optimization options then Opti.optimizeWithOption optimizeOption else id


parseProgramUntil :: [VariableName] -> Maybe Func -> [Symbol] -> Either ErrorString (Program, [Symbol])
parseProgramUntil _ Nothing [] = return ([],[])
parseProgramUntil _ (Just fnName) [] = Left $ "Function '" ++ fnName ++ "' has no body."
parseProgramUntil vs Nothing (x:xs)
  | x == ";" = parseProgramUntil vs Nothing xs
  | isViableName x = parseProgramUntil vs (Just x) xs
  | otherwise = Left $ "'" ++ x ++ "' is no viable Function name."
parseProgramUntil vs (Just fnName) ("=":xs) = do
  (expr, r1) <- parseUntil vs Nothing Nothing (Nothing, Just ";") xs
  (program, r2) <- parseProgramUntil [] Nothing r1
  return ((fnName, expr) : program, r2)
parseProgramUntil vs (Just fnName) (x:xs)
  | x == ";" = Left "Expected Variable name or '='."
  | isViableName x =  parseProgramUntil (vs ++ [x]) (Just fnName) xs
  | otherwise = Left $ "'" ++ x ++ "' is no viable Variable name."

