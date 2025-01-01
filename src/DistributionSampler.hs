module DistributionSampler
  ( SampledDensity (..),
    SampleInfo (..),
    sampleMass,
    sampleCumulative,
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
  let rt = Runtime {program, currentFnName = "main", arguments = [], recursionDepth = 0, maxRecursionDepth = 10000}
  samples <- fmap (fmap convertToFloat) (replicateM (numberOfSamples info) $ sampleRand rt mainExpr)
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
  let rt = Runtime {program, currentFnName = "main", arguments = [], recursionDepth = 0, maxRecursionDepth = 10000}
  samples <- fmap (fmap convertToFloat) (replicateM numberOfSamples $ sampleRand rt mainExpr)
  let masses =
        map (\(x, y) -> (x, y / fromIntegral numberOfSamples))
          . Map.toList
          . Map.fromListWith (+)
          . map (,1)
          $ samples
  return masses

sampleCumulative :: (MonadRandom m) => Program -> SampleInfo -> m [(Double, Double)]
sampleCumulative program info = do
  let mainExpr = unwrapMaybe $ lookup "main" program
  let rt = Runtime {program, currentFnName = "main", arguments = [], recursionDepth = 0, maxRecursionDepth = 10000}
  samples <- fmap (fmap convertToFloat) (replicateM (numberOfSamples info) $ sampleRand rt mainExpr)
  let indices = map (toBucketIndex info) samples
  let probabilities =
        convertToCDF info 0
          . Map.toList
          . Map.fromListWith (+)
          . map (,1)
          $ indices
  let (firstX,_) = head probabilities 
  let addFirst = (firstX - stepWidth info,0.0) : probabilities
  return $ makeRectangles addFirst

makeRectangles :: [(Double, Double)] -> [(Double, Double)]
makeRectangles (p1@(_,y1):p2@(x2,_):rest) = p1 : (x2,y1) : makeRectangles (p2 : rest)
makeRectangles list = list

convertToCDF :: SampleInfo -> Int -> [(BucketIndex, Int)] -> [(Double, Double)]
convertToCDF _ _ [] = []
convertToCDF info seenSamples ((x, count) : rest) =
  (fromBucketIndex info x + stepWidth info, amount / samples)
    : convertToCDF info (seenSamples + count) rest
  where
    amount = fromIntegral (seenSamples + count)
    samples = fromIntegral (numberOfSamples info)