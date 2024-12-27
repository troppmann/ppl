module Query
  ( QueryType (..),
    qInterpret,
  )
where

import Interpret
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

qInterpret :: Runtime -> Expr -> QueryType -> Either ErrorString DimensionalProbability
-- assumes only normalized distributions
qInterpret _ _ QAny = return (0, 1.0)
qInterpret rt expr (QIs bool) = interpret rt expr (VBool bool)
qInterpret  rt expr (QAt float) = interpret rt expr (VFloat float)
qInterpret  rt expr (QLt float) = interpret rt (LessThan expr (Const $ VFloat float)) (VBool True)
qInterpret  rt expr (QLe float) = interpret rt (LessThanOrEqual expr (Const $ VFloat float)) (VBool True)
qInterpret  rt expr (QGt float) = interpret rt (GreaterThan expr (Const $ VFloat float)) (VBool True)
qInterpret  rt expr (QGe float) = interpret rt (GreaterThanOrEqual expr (Const $ VFloat float)) (VBool True)
qInterpret  rt (CreateTuple e1 e2) (QTuple q1 q2) = do
  dimProb1 <- qInterpret rt e1 q1
  dimProb2 <- qInterpret rt e2 q2
  return $ dimProb1 #*# dimProb2
qInterpret  rt (IfThenElse boolExpr e1 e2) query@(QTuple _ _) = do
  dimProbTrue <- interpret rt boolExpr (VBool True)
  let dimProbFalse = (0, 1.0) #-# dimProbTrue
  dimProbBranchTrue <- qInterpret rt e1 query
  dimProbBranchFalse <- qInterpret rt e2 query
  return $ (dimProbTrue #*# dimProbBranchTrue) #+# (dimProbFalse #*# dimProbBranchFalse)
qInterpret  rt _ (QTuple _ _) = Left "Can't interpret singular value expression with a tuple query."
