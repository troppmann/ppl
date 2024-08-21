module DistributionSampler
  ( SampledDistribution (..),
    SampleInfo (..),
    sampleDistr,
  )
where

import Control.Monad (replicateM)
import Data.List
import Data.Map qualified as Map
import Representation
import Sample

data SampleInfo = SampleInfo
  { start :: Float,
    stepWidth :: Float,
    numberOfSamples :: Int
  }
  deriving (Show)

data SampledDistribution = SampledDistribution
  { info :: SampleInfo,
    buckets :: Map.Map Int Float
  }
  deriving (Show)

sampleDistr :: Expr -> SampleInfo -> IO (Map.Map Int Float)
sampleDistr expr info = do
  samples <- fmap (fmap convertToFloat) (replicateM (numberOfSamples info) $ sampleIO expr)
  let indices = map (toBucketIndex info) samples
  let sorted = group . sort $ indices
  let densities = map (toDensityEntry info) sorted
  return $ Map.fromList densities

convertToFloat :: Value -> Float
convertToFloat (VFloat f) = f
convertToFloat (VBool _) = error "Expected Float got Bool."

toDensityEntry :: SampleInfo -> [Int] -> (Int, Float)
toDensityEntry _ [] = error "Empty List"
toDensityEntry info sameIndexList@(x : _xs) = (x, density)
  where
    l = fromIntegral $ length sameIndexList
    samples = fromIntegral $ numberOfSamples info
    probability = l / samples
    density = probability / stepWidth info

type BucketIndex = Int

toBucketIndex :: SampleInfo -> Float -> BucketIndex
toBucketIndex info sample = index
  where
    index = floor $ (sample - start info) / stepWidth info
