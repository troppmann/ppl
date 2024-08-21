module Representation
  ( Expr (..),
    Value (..),
    Probability,
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
  | LessEqualThan Expr Expr
  | GreaterThan Expr Expr
  | GreaterEqualThan Expr Expr
  | IfElseThen Expr Expr Expr
  deriving (Show, Read, Eq)

data Value
  = VFloat Double
  | VBool Bool
  deriving (Show, Read, Eq)

type Probability = Double