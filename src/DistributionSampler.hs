module DistributionSampler
  ( SampledDensity (..),
    SampleInfo (..),
    sampleMass,
    convertToList,
    sampleDensity,
    density,
  )
where

import Control.Monad.Random
import Data.Bifunctor qualified
import Data.Map qualified as Map
import Data.Maybe
import Debug.Extended
import Representation
import Sample

data SampleInfo = SampleInfo
  { start :: Double,
    stepWidth :: Double,
    numberOfSamples :: Int
  }
  deriving (Show)

data SampledDensity = SampledDensity
  { info :: SampleInfo,
    buckets :: Map.Map Int Double
  }
  deriving (Show)

sampleDensity :: (MonadRandom m) => Program -> SampleInfo -> m [(Double, Double)]
sampleDensity program info = do
  let mainExpr = unwrapMaybe $ lookup "main" program
  samples <- fmap (fmap convertToFloat) (replicateM (numberOfSamples info) $ sampleRand (defaultSampleRuntime program) mainExpr)
  let indices = map (toBucketIndex info) samples
  let densities =
        map (\(x, y) -> (fromBucketIndex info x, y / fromIntegral (numberOfSamples info) / stepWidth info))
          . Map.toList
          . Map.fromListWith (+)
          . map (,1)
          $ indices
  return densities

convertToFloat :: Value -> Double
convertToFloat (VFloat f) = f
convertToFloat (VBool True) = 1.0
convertToFloat (VBool False) = 0.0
convertToFloat (VTuple _ _) = error "Expected Float got Tuple."

type BucketIndex = Int

toBucketIndex :: SampleInfo -> Double -> BucketIndex
toBucketIndex info sample = index
  where
    index = floor $ (sample - start info) / stepWidth info

fromBucketIndex :: SampleInfo -> BucketIndex -> Double
fromBucketIndex info index = start info + fromIntegral index * stepWidth info

density :: SampledDensity -> Double -> Double
density d value = fromMaybe 0.0 $ Map.lookup index $ buckets d
  where
    index = toBucketIndex (info d) value

convertToList :: SampledDensity -> [(Double, Double)]
convertToList sampledDis = map (Data.Bifunctor.first (fromBucketIndex (info sampledDis))) . Map.toList $ buckets sampledDis

sampleMass :: (MonadRandom m) => Program -> Int -> m [(Double, Double)]
sampleMass program numberOfSamples = do
  let mainExpr = unwrapMaybe $ lookup "main" program
  samples <- fmap (fmap convertToFloat) (replicateM numberOfSamples $ sampleRand (defaultSampleRuntime program) mainExpr)
  let masses =
        map (\(x, y) -> (x, y / fromIntegral numberOfSamples))
          . Map.toList
          . Map.fromListWith (+)
          . map (,1)
          $ samples
  return masses