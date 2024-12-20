{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module DistributionSampler
  ( SampledDensity (..),
    SampleInfo (..),
    sampleMass,
    convertToList,
    sampleDistr,
    density,
  )
where

import Control.Monad.Random
import Data.Bifunctor qualified
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

data SampledDensity = SampledDensity
  { info :: SampleInfo,
    buckets :: Map.Map Int Double
  }
  deriving (Show)

sampleDistr :: (MonadRandom m) => Expr -> SampleInfo -> m SampledDensity
sampleDistr expr info = do
  samples <- fmap (fmap convertToFloat) (replicateM (numberOfSamples info) $ sampleRand expr)
  let indices = map (toBucketIndex info) samples
  let sorted = group . sort $ indices
  let densities = map (toDensityEntry info) sorted
  return $ SampledDensity {info, buckets = Map.fromList densities}

convertToFloat :: Value -> Double
convertToFloat (VFloat f) = f
convertToFloat (VBool True) = 1.0
convertToFloat (VBool False) = 0.0
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
density :: SampledDensity -> Double -> Double
density d value = fromMaybe 0.0 $ Map.lookup index $ buckets d
  where
    index = toBucketIndex (info d) value

convertToList :: SampledDensity -> [(Double, Double)]
convertToList sampledDis = map (Data.Bifunctor.first (fromBucketIndex (info sampledDis))) . Map.toList $ buckets sampledDis

sampleMass :: (MonadRandom m) => Expr -> Int -> m [(Double, Double)]
sampleMass expr numberOfSamples = do
  samples <- fmap (fmap convertToFloat) (replicateM numberOfSamples $ sampleRand expr)
  let masses =
        map (\(x, y) -> (x, y / fromIntegral numberOfSamples))
          . Map.toList
          . Map.fromListWith (+)
          . map (\x -> (x, 1))
          $ samples
  return masses