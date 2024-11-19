module Query
  ( QueryType (..),
    qInterpret,
  )
where

import Interpret
import Representation

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

qInterpret :: Expr -> QueryType -> Either ErrorString DimensionalProbability
-- assumes only normalized distributions
qInterpret _ QAny = return (0, 1.0)
qInterpret expr (QIs bool) = interpret expr (VBool bool)
qInterpret expr (QAt float) = interpret expr (VFloat float)
qInterpret expr (QLt float) = interpret (LessThan expr (Const $ VFloat float)) (VBool True)
qInterpret expr (QLe float) = interpret (LessThanOrEqual expr (Const $ VFloat float)) (VBool True)
qInterpret expr (QGt float) = interpret (GreaterThan expr (Const $ VFloat float)) (VBool True)
qInterpret expr (QGe float) = interpret (GreaterThanOrEqual expr (Const $ VFloat float)) (VBool True)
qInterpret (CreateTuple e1 e2) (QTuple q1 q2) = do
  dimProb1 <- qInterpret e1 q1
  dimProb2 <- qInterpret e2 q2
  return $ dimProb1 #*# dimProb2
qInterpret (IfElseThen boolExpr e1 e2) query@(QTuple _ _) = do
  dimProbTrue <- interpret boolExpr (VBool True)
  let dimProbFalse = (0, 1.0) #-# dimProbTrue
  dimProbBranchTrue <- qInterpret e1 query
  dimProbBranchFalse <- qInterpret e2 query
  return $ (dimProbTrue #*# dimProbBranchTrue) #+# (dimProbFalse #*# dimProbBranchFalse)
qInterpret _ (QTuple _ _) = Left "Can't interpret singular value expression with a tuple query."
