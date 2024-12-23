module Representation
  ( Expr (..),
    Value (..),
    FnName,
    Program,
    wrapInMain,
    DimensionalProbability,
    Dimension,
    Probability,
    (#*#),
    (#+#),
    (#-#),
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
  | IfThenElse Expr Expr Expr
  | CreateTuple Expr Expr
  | Exponent Expr Expr
  | FnCall FnName [Expr]
  | FnParameter Int
  deriving (Show, Read, Eq)

type FnName = String


data Value
  = VFloat Double
  | VBool Bool
  | VTuple Value Value
  deriving (Show, Read, Eq)

type Program = [(FnName, Expr)]

wrapInMain :: Expr -> Program
wrapInMain expr = [("main", expr)]

type Probability = Double

type Dimension = Int

type DimensionalProbability = (Dimension, Probability)

infixl 7 #*#

(#*#) :: DimensionalProbability -> DimensionalProbability -> DimensionalProbability
(#*#) (dimA, probA) (dimB, probB) = (dimA + dimB, probA * probB)

infixl 6 #+#

(#+#) :: DimensionalProbability -> DimensionalProbability -> DimensionalProbability
(#+#) (_, 0.0) (_, 0.0) = (0, 0.0)
(#+#) dimProb (_, 0.0) = dimProb
(#+#) (_, 0.0) dimProb = dimProb
(#+#) (dimA, probA) (dimB, probB)
  | dimA < dimB = (dimA, probA)
  | dimA > dimB = (dimB, probB)
  | otherwise = (dimA, probA + probB)

infixl 6 #-#

(#-#) :: DimensionalProbability -> DimensionalProbability -> DimensionalProbability
(#-#) (_, 0.0) (_, 0.0) = (0, 0.0)
(#-#) dimProb (_, 0.0) = dimProb
-- TODO 16.11.24 is this clamping sound or should i just ignore the minus entirely
(#-#) (_, 0.0) (dim, _) = (dim, 0.0)
(#-#) (dimA, probA) (dimB, probB)
  | dimA < dimB = (dimA, probA)
  -- TODO 18.11.24 could also be probB or not?
  | dimA > dimB = (dimB, 0.0)
  -- TODO 18.11.24 could also just be an assert
  | otherwise = (dimA, max (probA - probB) 0)
