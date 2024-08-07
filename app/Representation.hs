module Representation
  ( Expr (..),
    Value (..),
    ControlFlow (..),
  )
where

data Expr
  = Const Value
  | Plus Expr Expr
  | Multiply Expr Expr
  | Subtract Expr Expr
  | Uniform
  | Normal
  deriving (Show, Read, Eq)

data Value
  = VFloat Float
  | VBool Bool
  deriving (Show, Read, Eq)

data ControlFlow = OpenParenthesis deriving (Show, Read, Eq)