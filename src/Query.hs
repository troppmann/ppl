module Query
  ( QueryType (..),
    qInfer,
    qInferProgram,
  )
where

import Debug.Extended
import Infer
import Representation
import Runtime

data QueryType
  = QAny -- other names: undefined anything unspecified blank uncertain
  | QIs Bool
  | QAt Double
  | QLt Double
  | QLe Double
  | QGt Double
  | QGe Double
  | QTuple QueryType QueryType
  deriving (Show, Read, Eq)

type ErrorString = String

qInferProgram :: Program -> QueryType -> Either ErrorString DimensionalProbability
qInferProgram program query = do
  mainExpr <- justOr (lookup "main" program) "main-Func not found."
  let rt = Runtime {program, arguments = [], currentFnName = "main", recursionDepth = 0, maxRecursionDepth = 100}
  qInfer rt mainExpr query

qInfer :: Runtime -> Expr -> QueryType -> Either ErrorString DimensionalProbability
-- assumes only normalized distributions
qInfer _ _ QAny = return (0, 1.0)
qInfer rt expr (QIs bool) = interpret rt expr (VBool bool)
qInfer rt expr (QAt float) = interpret rt expr (VFloat float)
qInfer rt expr (QLt float) = interpret rt (LessThan expr (Const $ VFloat float)) (VBool True)
qInfer rt expr (QLe float) = interpret rt (LessThanOrEqual expr (Const $ VFloat float)) (VBool True)
qInfer rt expr (QGt float) = interpret rt (GreaterThan expr (Const $ VFloat float)) (VBool True)
qInfer rt expr (QGe float) = interpret rt (GreaterThanOrEqual expr (Const $ VFloat float)) (VBool True)
qInfer rt (CreateTuple e1 e2) (QTuple q1 q2) = do
  dimProb1 <- qInfer rt e1 q1
  dimProb2 <- qInfer rt e2 q2
  return $ dimProb1 #*# dimProb2
qInfer rt (IfThenElse boolExpr e1 e2) query@(QTuple _ _) = do
  dimProbTrue <- interpret rt boolExpr (VBool True)
  let dimProbFalse = (0, 1.0) #-# dimProbTrue
  dimProbBranchTrue <- qInfer rt e1 query
  dimProbBranchFalse <- qInfer rt e2 query
  return $ (dimProbTrue #*# dimProbBranchTrue) #+# (dimProbFalse #*# dimProbBranchFalse)
qInfer _ _ (QTuple _ _) = return (0, 0.0)
