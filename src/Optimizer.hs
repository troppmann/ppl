module Optimizer
  ( optimize,
    optimizeExpr,
  )
where

import Control.Monad.State
import Control.Monad.Except
import Runtime
import Representation
import Debug.Extended

optimize :: Program -> Program
optimize program = map (\(fnName,expr) -> (fnName, unwrapEither $ optExpr fnName expr)) program
  where
    runTime fnName = InferRuntime {program, arguments=[], currentFnName=fnName, recursionDepth=0, maxRecursionDepth=10}
    optExpr fnName expr = runExcept $ evalStateT (optimizeExpr expr) (runTime fnName)


type ErrorString = String
type RuntimeState a = StateT InferRuntime (Except ErrorString) a

optimizeExpr :: Expr -> RuntimeState Expr
optimizeExpr (Plus e1 e2) = optimizeArithmetic e1 e2 Plus (+)
optimizeExpr (Subtract e1 e2) = optimizeArithmetic e1 e2 Subtract (-)
optimizeExpr (Multiply e1 e2) = optimizeArithmetic e1 e2 Multiply (*)
optimizeExpr (Divide e1 e2) = optimizeArithmetic e1 e2 Divide (/)
optimizeExpr (And e1 e2) = optimizeAnd e1 e2
optimizeExpr (Or e1 e2) = optimizeOr e1 e2
optimizeExpr (LessThan e1 e2) = optimizeCompareArithmetic e1 e2 LessThan (<)
optimizeExpr (LessThanOrEqual e1 e2) = optimizeCompareArithmetic e1 e2 LessThanOrEqual (<=)
optimizeExpr (GreaterThan e1 e2) = optimizeCompareArithmetic e1 e2 GreaterThan (>)
optimizeExpr (GreaterThanOrEqual e1 e2) = optimizeCompareArithmetic e1 e2 GreaterThanOrEqual (>=)
optimizeExpr (Equal e1 e2) = optimizeCompareArithmetic e1 e2 Equal (==)
optimizeExpr (Unequal e1 e2) = optimizeCompareArithmetic e1 e2 Unequal (/=)
optimizeExpr (Not e1) = optimizeNot e1
optimizeExpr (IfThenElse e1 e2 e3) = optimizeIfThenElse e1 e2 e3
optimizeExpr (CreateTuple e1 e2) = optimizeCreateTuple e1 e2
optimizeExpr (FnCall fnName argument) = optimizeFnCall fnName argument
optimizeExpr (FnParameter index) = optimizeFnParameter index
optimizeExpr expr = return expr

optimizeArithmetic :: Expr -> Expr -> (Expr -> Expr -> Expr) -> (Double -> Double -> Double) -> RuntimeState Expr
optimizeArithmetic e1 e2 makeExpr f = do
  opt1 <- optimizeExpr e1
  opt2 <- optimizeExpr e2
  case (opt1, opt2) of
    (Const (VFloat v1), Const (VFloat v2)) -> return $ Const $ VFloat $ f v1 v2
    _ -> return $ makeExpr opt1 opt2

optimizeOr :: Expr -> Expr -> RuntimeState Expr
optimizeOr e1 e2 = do
  opt1 <- optimizeExpr e1
  case opt1 of
    (Const (VBool True)) -> return $ Const (VBool True)
    (Const (VBool False)) -> optimizeExpr e2
    _ -> Or opt1 <$> optimizeExpr e2

optimizeAnd :: Expr -> Expr -> RuntimeState Expr
optimizeAnd e1 e2 = do
  opt1 <- optimizeExpr e1
  case opt1 of
    (Const (VBool True)) -> optimizeExpr e2
    (Const (VBool False)) -> return $ Const (VBool False)
    _ -> And opt1 <$> optimizeExpr e2

optimizeCompareArithmetic :: Expr -> Expr -> (Expr -> Expr -> Expr) -> (forall a. (Eq a, Ord a) => a -> a -> Bool) -> RuntimeState Expr
optimizeCompareArithmetic e1 e2 makeExpr f = do
  opt1 <- optimizeExpr e1
  opt2 <- optimizeExpr e2
  case (opt1, opt2) of
    (Const (VFloat v1), Const (VFloat v2)) -> return $ Const $ VBool $ f v1 v2
    (Const (VBool v1), Const (VBool v2)) -> return $ Const $ VBool $ f v1 v2
    _ -> return $ makeExpr opt1 opt2

optimizeNot :: Expr -> RuntimeState Expr
optimizeNot e1 = do
  opt1 <- optimizeExpr e1
  case opt1 of
    (Const (VBool v1)) -> return $ Const $ VBool $ not v1
    _ -> return $ Not opt1

optimizeIfThenElse :: Expr -> Expr -> Expr -> RuntimeState Expr
optimizeIfThenElse ifExpr thenExpr elseExpr = do
  optIf <- optimizeExpr ifExpr
  case optIf of
    (Const (VBool True)) -> optimizeExpr thenExpr
    (Const (VBool False)) -> optimizeExpr elseExpr
    _ -> IfThenElse optIf <$> optimizeExpr thenExpr <*> optimizeExpr elseExpr

optimizeCreateTuple :: Expr -> Expr -> RuntimeState Expr
optimizeCreateTuple e1 e2 = do
  opt1 <- optimizeExpr e1
  opt2 <- optimizeExpr e2
  return $ CreateTuple opt1 opt2

optimizeFnParameter :: Int -> RuntimeState Expr
optimizeFnParameter index = do
  rt <- get
  let parameter = getElem (arguments rt) index
  case parameter of
    (Just expr) -> return expr
    Nothing -> return $ FnParameter index
optimizeFnCall :: FnName -> [Expr] -> RuntimeState Expr
optimizeFnCall fnName args = do
  optArgs <- traverse optimizeExpr args
  rt <- get
  let newExpr = lookup fnName (program rt)
  case newExpr of
    (Just e1) -> do
      let oldDepth = recursionDepth rt
      if oldDepth + 1 > maxRecursionDepth rt then
        throwError "MaxRecursionDepth reached"
      else do
        let oldFnName = currentFnName rt
        let newRt = rt {recursionDepth=oldDepth + 1, arguments=optArgs, currentFnName=fnName}
        put newRt
        catchError (optimizeExpr e1) (\_ -> if fnName /= oldFnName || recursionDepth rt <= 0 then 
          return $ FnCall fnName args
          else throwError "backTrack cause of MaxRecursionDepth"
          )
    Nothing ->
      return $ FnCall fnName args


