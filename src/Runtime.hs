module Runtime
  ( InferRuntime (..),
    defaultInferRuntime,
  )
where
import Representation

data InferRuntime = InferRuntime
  { program :: Program,
    arguments :: [Expr],
    currentFnName :: String, 
    recursionDepth :: Int,
    maxRecursionDepth :: Int
  }
  deriving (Show, Eq)

defaultInferRuntime :: Program -> InferRuntime
defaultInferRuntime program = InferRuntime {program, arguments=[], currentFnName="main", recursionDepth=0, maxRecursionDepth=100}