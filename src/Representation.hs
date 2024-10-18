module Representation
  ( Expr (..),
    Value (..),
    DimensionalProbability,
    (⊙),
    (⊕),
  )
where

data Expr
  = Const Value
  | Plus Expr Expr
  | Multiply Expr Expr
  | Subtract Expr Expr
  | Divide Expr Expr
  | Uniform
  | Normal
  | And Expr Expr
  | Not Expr
  | Or Expr Expr
  | Equal Expr Expr
  | Unequal Expr Expr
  | LessThan Expr Expr
  | LessThanOrEqual Expr Expr
  | GreaterThan Expr Expr
  | GreaterThanOrEqual Expr Expr
  | IfElseThen Expr Expr Expr
  | CreateTuple Expr Expr
  deriving (Show, Read, Eq)

data Value
  = VFloat Double
  | VBool Bool
  | VTuple Value Value
  deriving (Show, Read, Eq)

type Probability = Double

type Dimension = Int

type DimensionalProbability = (Dimension, Probability)

(⊙) :: DimensionalProbability -> DimensionalProbability -> DimensionalProbability
(⊙) (dimA, probA) (dimB, probB) = (dimA + dimB, probA * probB)

(⊕) :: DimensionalProbability -> DimensionalProbability -> DimensionalProbability
(⊕) (_, 0.0) (_, 0.0) = (0, 0.0) -- is this right
(⊕) dimProb (_, 0.0) = dimProb
(⊕) (_, 0.0) dimProb = dimProb
(⊕) (dimA, probA) (dimB, probB)
  | dimA < dimB = (dimA, probA)
  | dimA > dimB = (dimB, probB)
  | otherwise = (dimA, probA + probB)
