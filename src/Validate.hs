module Validate
  ( ValidateInfo (..),
    validateExpr,
    validateFunction,
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
validateExpr info expr = validateFunction (start info) (end info) (stepWidth info) (convertExprToFunction expr)

convertExprToFunction :: Expr -> (Double -> Double)
convertExprToFunction expr = convertOutput . interpret expr . convertInput
  where
    convertInput = VFloat
    convertOutput = fromRight 0.0 . fmap snd

validateFunction :: Double -> Double -> Double -> (Double -> Double) -> Double
validateFunction start end stepWidth f
  | start > end = 0.0
  | otherwise = (trapez f start b) + validateFunction b end stepWidth f
  where
    b = start + stepWidth

trapez :: (Double -> Double) -> Double -> Double -> Double
trapez f a b = (b - a) * 0.5 * (f a + f b)
