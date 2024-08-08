module Representation
  ( Expr (..),
    Value (..),
  )
where

data Expr
  = Const Value
  | Plus Expr Expr
  | Multiply Expr Expr
  | Subtract Expr Expr
  | Uniform
  | Normal
  | And Expr Expr
  | Or Expr Expr
  | Equal Expr Expr
  | Unequal Expr Expr
  | LessThan Expr Expr
  | LessEqualThan Expr Expr
  | GreaterThan Expr Expr
  | GreaterEqualThan Expr Expr
  deriving (Show, Read, Eq)

data Value
  = VFloat Float
  | VBool Bool
  deriving (Show, Read, Eq)
