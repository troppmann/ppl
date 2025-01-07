{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ApproximateIntegration
  ( LinearSpacing (..),
    convertProgramToFunction,
    convertCumulativeToFunction,
    approxProgram,
    approxFunc,
    trapezTwoPoints,
  )
where

import Infer
import Representation
import Query

data LinearSpacing = LinearSpacing
  { start :: Double,
    end :: Double,
    stepWidth :: Double
  }
  deriving (Show)

-- only works on pdf's not pmf's
approxProgram :: LinearSpacing -> Program -> Double
approxProgram info program = approxFunc info (convertProgramToFunction program)

approxFunc :: LinearSpacing -> (Double -> Double) -> Double
approxFunc info = approxIntegral2 (start info) (end info) (stepWidth info)

convertProgramToFunction :: Program -> (Double -> Double)
convertProgramToFunction program =  dimProbToDouble . inferProgram program . doubleToValue
  where
    doubleToValue = VFloat
    dimProbToDouble = replaceForNanOrInf . either (const 0.0) snd

convertCumulativeToFunction :: Program -> (Double -> Double)
convertCumulativeToFunction program =  dimProbToDouble . qInferProgram program . doubleToQuery
  where
    doubleToQuery = QLe NormalMode
    dimProbToDouble = replaceForNanOrInf . either (const 0.0) snd

replaceForNanOrInf :: Double -> Double
replaceForNanOrInf value
  | isNaN value = 0.0
  | isInfinite value = 100000000
  | otherwise = value

-- TODO: f gets executed twice per step value
approxIntegral :: Double -> Double -> Double -> (Double -> Double) -> Double
approxIntegral start end stepWidth f
  | start > end = 0.0
  | otherwise = trapez f start b + approxIntegral b end stepWidth f
  where
    b = start + stepWidth

trapez :: (Double -> Double) -> Double -> Double -> Double
trapez f a b = (b - a) * 0.5 * (f a + f b)

trapezTwoPoints :: (Double, Double) -> (Double, Double) -> Double
trapezTwoPoints (x1, y1) (x2 , y2) = (x2 - x1) * 0.5 * (y1 + y2)

-- TODO: don't execute function twice but utterly unreadable
-- SpeedUp is measurable
approxIntegral2 :: Double -> Double -> Double -> (Double -> Double) -> Double
approxIntegral2 start end stepWidth f = factor * fst (foldr combine (0, 0) values)
  where
    values = generateFunctionValues start end stepWidth f
    factor = 0.5 * stepWidth

combine :: Double -> (Double, Double) -> (Double, Double)
combine listEntry (acc, previous) = (acc + previous + listEntry, listEntry)

generateFunctionValues :: Double -> Double -> Double -> (Double -> Double) -> [Double]
generateFunctionValues start end stepWidth f = map f $ take times $ iterate (+ stepWidth) start
  where
    times = ceiling $ (end - start) / stepWidth