module Representation
  ( Expr (..),
    Value (..),
    DimensionalProbability,
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
  deriving (Show, Read, Eq)

data Value
  = VFloat Double
  | VBool Bool
  deriving (Show, Read, Eq)

type Probability = Double

type Dimension = Int

type DimensionalProbability = (Dimension, Probability)