module Runtime
  ( InferRuntime (..),
    defaultInferRuntime,
  )
where
import Representation

data InferRuntime = InferRuntime
  { program :: Program,
    arguments :: [Expr],
    recursionDepth :: Int,
    maxRecursionDepth :: Int
  }
  deriving (Show, Eq)

defaultInferRuntime :: Program -> InferRuntime
defaultInferRuntime program = InferRuntime {program, arguments=[], recursionDepth=0, maxRecursionDepth=100}