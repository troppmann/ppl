{-# LANGUAGE DuplicateRecordFields #-}

module Chart ( createPoints
, plotDistrSvg) where

import Representation
import ApproximateIntegration (LinearSpacing(..), convertExprToFunction, trapezTwoPoints)
import DistributionSampler
import qualified ApproximateIntegration as LinearSpacing
import Debug.Extended
import Control.Monad.Random
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
import Parser.String

type Point2D = (Double, Double)
createPoints :: Expr -> LinearSpacing -> [Point2D]
createPoints expr spacing = addYValues (convertExprToFunction expr) (xValues spacing)

xValues :: LinearSpacing -> [Double]
xValues spacing = [(LinearSpacing.start spacing), second..(end spacing)]
    where second = LinearSpacing.start spacing + LinearSpacing.stepWidth spacing


addYValues :: (Double -> Double) -> [Double] -> [Point2D]
addYValues func = map (\x -> (x, func x))

type FileName = String
type NumberOfSamples = Int
plotDistrSvg :: FileName ->  Expr -> LinearSpacing ->NumberOfSamples -> IO ()
plotDistrSvg filename expr spacing numberOfSamples = do
  let interpretedLine = createPoints expr spacing
  let approx = approxI (tail interpretedLine) (head interpretedLine)
  sampledDis <- evalRandIO (sampleDistr expr SampleInfo {start = LinearSpacing.start spacing, stepWidth = LinearSpacing.stepWidth spacing, numberOfSamples})
  let sampledBars = map (\(x,y) -> (x + 0.5 * LinearSpacing.stepWidth spacing, [y])) $ convertToList sampledDis

  toFile def filename $ do
    layout_title .= "PDF  \n " ++ toString expr
    setColors [opaque blue, opaque red]
    plot $ plotBars <$> normalBars ["Sampled"] sampledBars
    plot (line ("Inferred ~Area: " ++ showFloatN approx 5) [interpretedLine])

normalBars :: (PlotValue x, BarsPlotValue y) => [String] -> [(x,[y])] -> EC l (PlotBars x y)
normalBars titles vals = liftEC $ do
    styles <- sequence [fmap mkStyle takeColor | _ <- titles]
    plot_bars_titles .= titles
    plot_bars_values .= vals
    plot_bars_style .= BarsClustered
    plot_bars_spacing .= BarsFixGap 0 0
    plot_bars_item_styles .= styles
  where
    mkStyle c = (solidFillStyle c, Nothing)

approxI :: [(Double, Double)] -> (Double, Double) -> Double
approxI [] _point = 0.0
approxI (x:xs) point = trapezTwoPoints point x + approxI xs x