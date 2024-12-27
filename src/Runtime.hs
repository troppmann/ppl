module Runtime
  ( Runtime (..),
    defaultRuntime,
  )
where
import Representation

data Runtime = Runtime
  { program :: Program,
    arguments :: [Expr],
    currentFnName :: String, 
    recursionDepth :: Int,
    maxRecursionDepth :: Int
  }
  deriving (Show, Eq)

defaultRuntime :: Program -> Runtime
defaultRuntime program = Runtime {program, arguments=[], currentFnName="main", recursionDepth=0, maxRecursionDepth=100}