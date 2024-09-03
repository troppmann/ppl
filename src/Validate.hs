{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Validate
  ( LinearSpacing (..),
    validateExpr,
    validateFunc,
  )
where

import Data.Either
import Interpret
import Representation

-- TODO: find better name
data LinearSpacing = LinearSpacing
  { start :: Double,
    end :: Double,
    stepWidth :: Double
  }
  deriving (Show)

-- only works on pdf's not pmf's
validateExpr :: LinearSpacing -> Expr -> Double
validateExpr info expr = validateFunc info (convertExprToFunction expr)

validateFunc :: LinearSpacing -> (Double -> Double) -> Double
validateFunc info f = approxIntegral2 (start info) (end info) (stepWidth info) f

convertExprToFunction :: Expr -> (Double -> Double)
convertExprToFunction expr = convertOutput . interpret expr . convertInput
  where
    convertInput = VFloat
    convertOutput = fromRight 0.0 . fmap snd

-- TODO: f gets executed twice per step value
approxIntegral :: Double -> Double -> Double -> (Double -> Double) -> Double
approxIntegral start end stepWidth f
  | start > end = 0.0
  | otherwise = trapez f start b + approxIntegral b end stepWidth f
  where
    b = start + stepWidth

trapez :: (Double -> Double) -> Double -> Double -> Double
trapez f a b = (b - a) * 0.5 * (f a + f b)

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