module Runtime
  ( InferRuntime (..),
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