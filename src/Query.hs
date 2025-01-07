module Query
  ( QueryType (..),
    QueryMode(..),
    qInfer,
    qInferProgram,
  )
where

import Control.Monad
import Debug.Extended
import Evaluate
import Infer
import Optimizer
import Representation
import Runtime

data QueryMode = NormalMode | Given deriving (Show, Read, Eq)

data QueryType
  = QAny 
  | QBool QueryMode Bool
  | QFloat QueryMode Double
  | QLt QueryMode Double
  | QLe QueryMode Double
  | QGt QueryMode Double
  | QGe QueryMode Double
  | QTuple QueryType QueryType
  deriving (Show, Read, Eq)

type ErrorString = String

qInferProgram :: Program -> QueryType -> Either ErrorString DimensionalProbability
qInferProgram program query = do
  mainExpr <- justOr (lookup "main" program) "main-Func not found."
  let rt = Runtime {program, arguments = [], currentFnName = "main", recursionDepth = 0, maxRecursionDepth = 100}
  (dimNormal, probNormal) <- qInfer rt mainExpr NormalMode query 
  (_dimCon, probCon) <- qInfer rt mainExpr Given query
  return (dimNormal, probNormal / probCon)

qInfer :: Runtime -> Expr -> QueryMode -> QueryType -> Either ErrorString DimensionalProbability
-- assumes only normalized distributions
qInfer _ _ _ QAny = return (0, 1.0)
qInfer rt expr Given (QBool NormalMode bool) = return (0, 1.0)
qInfer rt expr _ (QBool _ bool) = infer rt expr (VBool bool)
qInfer rt expr Given (QFloat NormalMode float) = return (0, 1.0)
qInfer rt expr _ (QFloat _ float) = infer rt expr (VFloat float)
qInfer rt expr Given (QLt NormalMode float) = return (0, 1.0)
qInfer rt expr _ (QLt _ float) = infer rt (LessThan expr (Const $ VFloat float)) (VBool True)
qInfer rt expr Given (QLe NormalMode float) = return (0, 1.0)
qInfer rt expr _ (QLe _ float) = infer rt (LessThanOrEqual expr (Const $ VFloat float)) (VBool True)
qInfer rt expr Given (QGt NormalMode float) = return (0, 1.0)
qInfer rt expr _ (QGt _ float) = infer rt (GreaterThan expr (Const $ VFloat float)) (VBool True)
qInfer rt expr Given (QGe NormalMode float) = return (0, 1.0)
qInfer rt expr _ (QGe _ float) = infer rt (GreaterThanOrEqual expr (Const $ VFloat float)) (VBool True)
qInfer rt (CreateTuple e1 e2) mode (QTuple q1 q2) = do
  dimProb1 <- qInfer rt e1 mode q1
  dimProb2 <- qInfer rt e2 mode q2
  return $ dimProb1 #*# dimProb2
qInfer rt (IfThenElse boolExpr e1 e2) mode query@(QTuple _ _) = do
  dimProbTrue <- infer rt boolExpr (VBool True)
  dimProbFalse <- infer rt boolExpr (VBool False)
  dimProbBranchTrue <- qInfer rt e1 mode query
  dimProbBranchFalse <- qInfer rt e2 mode query
  return $ (dimProbTrue #*# dimProbBranchTrue) #+# (dimProbFalse #*# dimProbBranchFalse)
qInfer rt (FnCall fnName arguments) mode query = do
  expr <- justOr (lookup fnName (program rt)) ("Fn '" ++ fnName ++ "' not found.")
  let newDepth = 1 + recursionDepth rt
  args <- traverse (optimizeExpr rt {recursionDepth = 0, maxRecursionDepth = 1} <=< replaceFnParameterWithContent rt) arguments
  let newRt = rt {recursionDepth = newDepth, arguments = args}
  if recursionDepth rt >= maxRecursionDepth rt
    then
      return (0, 0.0)
    else
      qInfer newRt expr mode query
qInfer _ _ _ (QTuple _ _) = return (0, 0.0)

