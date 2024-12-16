{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ApproximateIntegration
  ( LinearSpacing (..),
    convertExprToFunction,
    approxExpr,
    approxFunc,
    trapezTwoPoints,
  )
where

import Data.Either
import Interpret
import Representation

data LinearSpacing = LinearSpacing
  { start :: Double,
    end :: Double,
    stepWidth :: Double
  }
  deriving (Show)

-- only works on pdf's not pmf's
approxExpr :: LinearSpacing -> Expr -> Double
approxExpr info expr = approxFunc info (convertExprToFunction expr)

approxFunc :: LinearSpacing -> (Double -> Double) -> Double
approxFunc info f = approxIntegral2 (start info) (end info) (stepWidth info) f

convertExprToFunction :: Expr -> (Double -> Double)
convertExprToFunction expr = convertOutput . interpret expr . convertInput
  where
    convertInput = VFloat
    convertOutput = replaceForNanOrInf . fromRight 0.0 . fmap snd

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