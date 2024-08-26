module DistributionSampler
  ( SampledDistribution (..),
    SampleInfo (..),
    sampleDistr,
    density,
  )
where

import Control.Monad.Random
import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Representation
import Sample

data SampleInfo = SampleInfo
  { start :: Double,
    stepWidth :: Double,
    numberOfSamples :: Int
  }
  deriving (Show)

data SampledDistribution = SampledDistribution
  { info :: SampleInfo,
    buckets :: Map.Map Int Double
  }
  deriving (Show)

sampleDistr :: (MonadRandom m) => Expr -> SampleInfo -> m SampledDistribution
sampleDistr expr info = do
  samples <- fmap (fmap convertToFloat) (replicateM (numberOfSamples info) $ sampleIO expr)
  let indices = map (toBucketIndex info) samples
  let sorted = group . sort $ indices
  let densities = map (toDensityEntry info) sorted
  return $ SampledDistribution {info, buckets = Map.fromList densities}

convertToFloat :: Value -> Double
convertToFloat (VFloat f) = f
convertToFloat (VBool _) = error "Expected Float got Bool."

toDensityEntry :: SampleInfo -> [Int] -> (Int, Double)
toDensityEntry _ [] = error "Empty List"
toDensityEntry info sameIndexList@(x : _xs) = (x, probDensity)
  where
    l = fromIntegral $ length sameIndexList
    samples = fromIntegral $ numberOfSamples info
    probability = l / samples
    probDensity = probability / stepWidth info

type BucketIndex = Int

toBucketIndex :: SampleInfo -> Double -> BucketIndex
toBucketIndex info sample = index
  where
    index = floor $ (sample - start info) / stepWidth info

-- TODO: maybe implement Distribution from Statistics lib
density :: SampledDistribution -> Double -> Double
density d value = fromMaybe 0.0 $ Map.lookup index $ buckets d
  where
    index = toBucketIndex (info d) value

-- TODO: ppl like "if Uniform > 0.5 then Uniform else 2.0" are both 1 and 0
-- idk how to handle these
type ProbabilityDimension = Int

dimension :: Expr -> ProbabilityDimension
dimension (Const _) = 0
dimension (Plus e1 e2)
  | c1 == c2 = c1
  | c1 > c2 = c2
  | c1 < c2 = c1
  where
    c1 = dimension e1
    c2 = dimension e2
