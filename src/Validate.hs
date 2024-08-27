module Validate
  ( ValidateInfo (..),
    validateExpr,
    validateFunc,
  )
where

import Data.Either
import Interpret
import Representation

-- TODO: find better name
data ValidateInfo = ValidateInfo
  { start :: Double,
    end :: Double,
    stepWidth :: Double
  }
  deriving (Show)

validateExpr :: ValidateInfo -> Expr -> Double
validateExpr info expr = validateFunc info (convertExprToFunction expr)

validateFunc :: ValidateInfo -> (Double -> Double) -> Double
validateFunc info f = approxIntegral (start info) (end info) (stepWidth info) f

convertExprToFunction :: Expr -> (Double -> Double)
convertExprToFunction expr = convertOutput . interpret expr . convertInput
  where
    convertInput = VFloat
    convertOutput = fromRight 0.0 . fmap snd

approxIntegral :: Double -> Double -> Double -> (Double -> Double) -> Double
approxIntegral start end stepWidth f
  | start > end = 0.0
  | otherwise = (trapez f start b) + approxIntegral b end stepWidth f
  where
    b = start + stepWidth

trapez :: (Double -> Double) -> Double -> Double -> Double
trapez f a b = (b - a) * 0.5 * (f a + f b)
