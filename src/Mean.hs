module Mean
  ( meanExpr,
  )
where

import Debug.Extended
import Interpret
import Representation

meanExpr :: Expr -> Either String Double
meanExpr Normal = return 0.0
meanExpr Uniform = return 0.5
meanExpr (Const (VFloat value)) = return value
meanExpr (Plus e1 e2) = do
  m1 <- meanExpr e1
  m2 <- meanExpr e2
  return $ m1 + m2
meanExpr (Subtract e1 e2) = do
  m1 <- meanExpr e1
  m2 <- meanExpr e2
  return $ m1 - m2
meanExpr (Multiply e1 e2) = do
  m1 <- meanExpr e1
  m2 <- meanExpr e2
  return $ m1 * m2
meanExpr (Divide e1 e2) = do
  m1 <- meanExpr e1
  m2 <- meanExpr e2
  return $ m1 / m2
meanExpr (IfElseThen condExpr branchE1 branchE2) = do
  (_dim, prob) <- interpret condExpr (VBool True)
  let probFalse = 1 - prob
  m1 <- meanExpr branchE1
  m2 <- meanExpr branchE2
  return $ prob * m1 + probFalse * m2
meanExpr e = todo $ "Missing meanExpr case" <> show e
