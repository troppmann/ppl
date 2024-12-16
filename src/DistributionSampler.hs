module DistributionSampler
  ( SampledDistribution (..),
    SampleInfo (..),
    convertToList,
    sampleDistr,
    density,
  )
where

import Control.Monad.Random
import Data.Map qualified as Map
import Data.Maybe
import Data.List
import Representation
import Sample
import qualified Data.Bifunctor

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
  samples <- fmap (fmap convertToFloat) (replicateM (numberOfSamples info) $ sampleRand expr)
  let indices = map (toBucketIndex info) samples
  let sorted = group . sort $ indices
  let densities = map (toDensityEntry info) sorted
  return $ SampledDistribution {info, buckets = Map.fromList densities}

convertToFloat :: Value -> Double
convertToFloat (VFloat f) = f
convertToFloat (VBool _) = error "Expected Float got Bool."
convertToFloat (VTuple _ _) = error "Expected Float got Tuple."

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

fromBucketIndex :: SampleInfo -> BucketIndex -> Double
fromBucketIndex info index = start info + fromIntegral index * stepWidth info

-- TODO: maybe implement Distribution from Statistics lib
density :: SampledDistribution -> Double -> Double
density d value = fromMaybe 0.0 $ Map.lookup index $ buckets d
  where
    index = toBucketIndex (info d) value

convertToList :: SampledDistribution -> [(Double, Double)]
convertToList sampledDis = map (Data.Bifunctor.first (fromBucketIndex (info sampledDis))) . Map.toList $ buckets sampledDis
