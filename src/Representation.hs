module Representation
  ( Expr (..),
    Value (..),
    FnName,
    Program,
    wrapInMain,
    addCustomFnToProgram,
    SampleFn (..),
    CumulativeFn (..),
    InferFn (..),
    DimensionalProbability,
    Dimension,
    Probability,
    (#*#),
    (#+#),
    (#-#),
  )
where

import Control.Monad.Random

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
  | Abs Expr
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
  | Custom InferFn CumulativeFn SampleFn
  deriving (Show, Read, Eq)

type FnName = String

data Value
  = VFloat Double
  | VBool Bool
  | VMar
  | VTuple Value Value
  deriving (Show, Read, Eq)

type Program = [(FnName, Expr)]

wrapInMain :: Expr -> Program
wrapInMain expr = [("main", expr)]

addCustomFnToProgram :: FnName -> (Value -> DimensionalProbability) -> (Double -> Probability) -> (forall m. (MonadRandom m) => m Value) -> Program -> Program
addCustomFnToProgram fnName inferFn cumulativeFn sampleFn program =
  (fnName, customNode) : program
  where
    customNode = Custom (InferFn inferFn) (CumulativeFn cumulativeFn) (SampleFn sampleFn)

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

newtype InferFn = InferFn (Value -> DimensionalProbability)

instance Show InferFn where
  show _ = "InferFn"

instance Read InferFn where
  readsPrec _ = error "Can't read Custom."

instance Eq InferFn where
  (==) = error "Can't compare Custom."

newtype CumulativeFn = CumulativeFn (Double -> Probability)

instance Show CumulativeFn where
  show _ = "CumulativeFn"

instance Read CumulativeFn where
  readsPrec _ = error "Can't read Custom."

instance Eq CumulativeFn where
  (==) = error "Can't compare Custom."

newtype SampleFn = SampleFn (forall m. (MonadRandom m) => m Value)

instance Show SampleFn where
  show _ = "SampleFn"

instance Read SampleFn where
  readsPrec _ = error "Can't read"

instance Eq SampleFn where
  (==) = error "Can't compare Custom."
