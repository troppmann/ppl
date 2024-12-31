module Optimizer
  ( optimize,
    optimizeExpr,
    optimizeWithOption,
    OptimizeOption (..),
  )
where

import Debug.Extended
import Representation
import Representation (Expr (Multiply))
import Runtime

optimize :: Program -> Program
optimize = optimizeWithOption OptimizeOption {maxLoopUnroll = 30}

data OptimizeOption = OptimizeOption {maxLoopUnroll :: Int}

optimizeWithOption :: OptimizeOption -> Program -> Program
optimizeWithOption option program = map (\(fnName, expr) -> (fnName, unwrapEither $ optExpr fnName expr)) program
  where
    createRtWithFnName fnName = Runtime {program, arguments = [], currentFnName = fnName, recursionDepth = 0, maxRecursionDepth = maxLoopUnroll option}
    optExpr fnName = optimizeExpr (createRtWithFnName fnName)

type ErrorString = String

optimizeExpr :: Runtime -> Expr -> Either ErrorString Expr
optimizeExpr rt (Plus e1 e2) = optimizePlus rt e1 e2
optimizeExpr rt (Subtract e1 e2) = optimizeSubtract rt e1 e2
optimizeExpr rt (Multiply e1 e2) = optimizeMultiply rt e1 e2
optimizeExpr rt (Divide e1 e2) = optimizeDivide rt e1 e2
optimizeExpr rt (Exponent e1 e2) = optimizeExponent rt e1 e2
optimizeExpr rt (And e1 e2) = optimizeAnd rt e1 e2
optimizeExpr rt (Or e1 e2) = optimizeOr rt e1 e2
optimizeExpr rt (LessThan e1 e2) = optimizeCompareArithmetic rt e1 e2 LessThan (<)
optimizeExpr rt (LessThanOrEqual e1 e2) = optimizeCompareArithmetic rt e1 e2 LessThanOrEqual (<=)
optimizeExpr rt (GreaterThan e1 e2) = optimizeCompareArithmetic rt e1 e2 GreaterThan (>)
optimizeExpr rt (GreaterThanOrEqual e1 e2) = optimizeCompareArithmetic rt e1 e2 GreaterThanOrEqual (>=)
optimizeExpr rt (Equal e1 e2) = optimizeCompareArithmetic rt e1 e2 Equal (==)
optimizeExpr rt (Unequal e1 e2) = optimizeCompareArithmetic rt e1 e2 Unequal (/=)
optimizeExpr rt (Not e1) = optimizeNot rt e1
optimizeExpr rt (IfThenElse e1 e2 e3) = optimizeIfThenElse rt e1 e2 e3
optimizeExpr rt (CreateTuple e1 e2) = optimizeCreateTuple rt e1 e2
optimizeExpr rt (FnCall fnName argument) = optimizeFnCall rt fnName argument
optimizeExpr rt (FnParameter index) = optimizeFnParameter rt index
optimizeExpr _ expr = return expr

optimizePlus :: Runtime -> Expr -> Expr -> Either ErrorString Expr
optimizePlus rt e1 e2 = do
  opt1 <- optimizeExpr rt e1
  opt2 <- optimizeExpr rt e2
  case (opt1, opt2) of
    (_, Const (VFloat 0.0)) -> return opt1
    (Const (VFloat 0.0), _) -> return opt2
    (Const (VFloat v1), Const (VFloat v2)) -> return $ Const $ VFloat (v1 + v2)
    ((Plus l (Const (VFloat vInner))), Const (VFloat v2)) -> return $ Plus l (Const $ VFloat (vInner + v2))
    ((Plus (Const (VFloat vInner)) r), Const (VFloat v2)) -> return $ Plus r (Const $ VFloat (vInner + v2))
    (Const (VFloat v1), (Plus l (Const (VFloat vInner)))) -> return $ Plus (Const $ VFloat (vInner + v1)) l
    (Const (VFloat v1), (Plus (Const (VFloat vInner)) r)) -> return $ Plus (Const $ VFloat (vInner + v1)) r
    (_, _) -> return $ Plus opt1 opt2

optimizeSubtract :: Runtime -> Expr -> Expr -> Either ErrorString Expr
optimizeSubtract rt e1 e2 = do
  opt1 <- optimizeExpr rt e1
  opt2 <- optimizeExpr rt e2
  case (opt1, opt2) of
    (_, Const (VFloat 0.0)) -> return opt1
    (Const (VFloat v1), Const (VFloat v2)) -> return $ Const $ VFloat (v1 - v2)
    (_, _) -> return $ Subtract opt1 opt2

optimizeMultiply :: Runtime -> Expr -> Expr -> Either ErrorString Expr
optimizeMultiply rt e1 e2 = do
  opt1 <- optimizeExpr rt e1
  opt2 <- optimizeExpr rt e2
  case (opt1, opt2) of
    (_, Const (VFloat 0.0)) -> return (Const (VFloat 0.0))
    (Const (VFloat 0.0), _) -> return (Const (VFloat 0.0))
    (_, Const (VFloat 1.0)) -> return opt1
    (Const (VFloat 1.0), _) -> return opt2
    (Const (VFloat v1), Const (VFloat v2)) -> return $ Const $ VFloat (v1 * v2)
    (Multiply l (Const (VFloat vInner)), Const (VFloat v2)) -> return $ Multiply l (Const $ VFloat (vInner * v2))
    (Plus l (Const (VFloat vInner)), Const (VFloat v2)) -> do
      optMult <- optimizeExpr rt $ Multiply l (Const (VFloat v2))
      return $ Plus optMult (Const $ VFloat (vInner * v2))
    (Multiply (Const (VFloat vInner)) r, Const (VFloat v2)) -> return $ Multiply r (Const $ VFloat (vInner * v2))
    (Plus (Const (VFloat vInner)) r, Const (VFloat v2)) -> do
      optMult <- optimizeExpr rt $ Multiply r (Const (VFloat v2))
      return $ Plus optMult (Const $ VFloat (vInner * v2))
    (Const (VFloat v1), Multiply l (Const (VFloat vInner))) -> return $ Multiply (Const $ VFloat (vInner * v1)) l
    (Const (VFloat v1), Plus l (Const (VFloat vInner))) -> do
      optMult <- optimizeExpr rt $ Multiply (Const $ VFloat v1) l
      return $ Plus (Const $ VFloat (vInner * v1)) optMult
    (Const (VFloat v1), Multiply (Const (VFloat vInner)) r) -> return $ Multiply (Const $ VFloat (vInner * v1)) r
    (Const (VFloat v1), Plus (Const (VFloat vInner)) r) -> do
      optMult <- optimizeExpr rt $ Multiply (Const $ VFloat v1) r
      return $ Plus (Const $ VFloat (vInner * v1)) optMult
    (_, _) -> return $ Multiply opt1 opt2

optimizeDivide :: Runtime -> Expr -> Expr -> Either ErrorString Expr
optimizeDivide rt e1 e2 = do
  opt1 <- optimizeExpr rt e1
  opt2 <- optimizeExpr rt e2
  case (opt1, opt2) of
    (_, Const (VFloat 1.0)) -> return opt1
    (Const (VFloat v1), Const (VFloat v2)) -> return $ Const $ VFloat (v1 / v2)
    (_, _) -> return $ Divide opt1 opt2

optimizeExponent :: Runtime -> Expr -> Expr -> Either ErrorString Expr
optimizeExponent rt e1 e2 = do
  opt1 <- optimizeExpr rt e1
  opt2 <- optimizeExpr rt e2
  case (opt1, opt2) of
    (_, Const (VFloat 1.0)) -> return opt1
    (Const (VFloat v1), Const (VFloat v2)) -> return $ Const $ VFloat (v1 ** v2)
    (_, _) -> return $ Divide opt1 opt2

optimizeOr :: Runtime -> Expr -> Expr -> Either ErrorString Expr
optimizeOr rt e1 e2 = do
  opt1 <- optimizeExpr rt e1
  opt2 <- optimizeExpr rt e2
  case (opt1, opt2) of
    (_, (Const (VBool True))) -> return $ Const (VBool True)
    ((Const (VBool True)), _) -> return $ Const (VBool True)
    (_, (Const (VBool False))) -> return opt1
    ((Const (VBool False)), _) -> return opt2
    (_, _) -> return $ Or opt1 opt2

optimizeAnd :: Runtime -> Expr -> Expr -> Either ErrorString Expr
optimizeAnd rt e1 e2 = do
  opt1 <- optimizeExpr rt e1
  opt2 <- optimizeExpr rt e2
  case (opt1, opt2) of
    ((Const (VBool False)), _) -> return $ Const (VBool False)
    (_, (Const (VBool False))) -> return $ Const (VBool False)
    (_, (Const (VBool True))) -> return opt1
    ((Const (VBool True)), _) -> return opt2
    _ -> return $ And opt1 opt2

optimizeCompareArithmetic :: Runtime -> Expr -> Expr -> (Expr -> Expr -> Expr) -> (forall a. (Eq a, Ord a) => a -> a -> Bool) -> Either ErrorString Expr
optimizeCompareArithmetic rt e1 e2 makeExpr f = do
  opt1 <- optimizeExpr rt e1
  opt2 <- optimizeExpr rt e2
  case (opt1, opt2) of
    (Const (VFloat v1), Const (VFloat v2)) -> return $ Const $ VBool $ f v1 v2
    (Const (VBool v1), Const (VBool v2)) -> return $ Const $ VBool $ f v1 v2
    _ -> return $ makeExpr opt1 opt2

optimizeNot :: Runtime -> Expr -> Either ErrorString Expr
optimizeNot rt e1 = do
  opt1 <- optimizeExpr rt e1
  case opt1 of
    (Const (VBool v1)) -> return $ Const $ VBool $ not v1
    _ -> return $ Not opt1

optimizeIfThenElse :: Runtime -> Expr -> Expr -> Expr -> Either ErrorString Expr
optimizeIfThenElse rt ifExpr thenExpr elseExpr = do
  optIf <- optimizeExpr rt ifExpr
  case optIf of
    (Const (VBool True)) -> optimizeExpr rt thenExpr
    (Const (VBool False)) -> optimizeExpr rt elseExpr
    _ -> do
      opt1 <- optimizeExpr rt thenExpr
      opt2 <- optimizeExpr rt elseExpr
      return $ IfThenElse optIf opt1 opt2

optimizeCreateTuple :: Runtime -> Expr -> Expr -> Either ErrorString Expr
optimizeCreateTuple rt e1 e2 = do
  opt1 <- optimizeExpr rt e1
  opt2 <- optimizeExpr rt e2
  return $ CreateTuple opt1 opt2

optimizeFnParameter :: Runtime -> Int -> Either ErrorString Expr
optimizeFnParameter rt index = do
  let parameter = getElem (arguments rt) index
  case parameter of
    (Just expr) -> return expr
    Nothing -> return $ FnParameter index

optimizeFnCall :: Runtime -> FnName -> [Expr] -> Either ErrorString Expr
optimizeFnCall rt fnName args = do
  optArgs <- traverse (optimizeExpr rt) args
  let newExpr = lookup fnName (program rt)
  case newExpr of
    (Just e1) -> do
      let oldDepth = recursionDepth rt
      if oldDepth > maxRecursionDepth rt
        then
          Left "MaxRecursionDepth reached"
        else do
          let oldFnName = currentFnName rt
          let newRt = rt {recursionDepth = oldDepth + 1, arguments = optArgs, currentFnName = fnName}
          let fnCall = optimizeExpr newRt e1
          case fnCall of
            (Left _) ->
              ( if fnName /= oldFnName || oldDepth <= 0
                  then
                    return $ FnCall fnName optArgs
                  else Left "backTrack cause of MaxRecursionDepth"
              )
            (Right expr) -> return expr
    Nothing ->
      return $ FnCall fnName args
