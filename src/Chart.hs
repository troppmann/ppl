{-# LANGUAGE DuplicateRecordFields #-}

module Chart
  ( createPoints,
    plotDensityToFile,
    plotMassToFile,
    plotCumulativeToFile,
  )
where

import ApproximateIntegration (LinearSpacing (..), convertProgramToFunction, convertCumulativeToFunction, trapezTwoPoints)
import ApproximateIntegration qualified as LinearSpacing
import Control.Monad.Random
import Debug.Extended
import DistributionSampler
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy
import Infer
import Representation

type Point2D = (Double, Double)

createPoints :: Program -> LinearSpacing -> [Point2D]
createPoints program spacing = addYValues (convertProgramToFunction program) (xValues spacing)

xValues :: LinearSpacing -> [Double]
xValues spacing = [(LinearSpacing.start spacing), second .. (end spacing)]
  where
    second = LinearSpacing.start spacing + LinearSpacing.stepWidth spacing

addYValues :: (Double -> Double) -> [Double] -> [Point2D]
addYValues func = map (\x -> (x, func x))

type FileName = String

type NumberOfSamples = Int

plotDensityToFile :: FileName -> Program -> LinearSpacing -> NumberOfSamples -> IO ()
plotDensityToFile filename program spacing numberOfSamples = do
  let inferedLine = createPoints program spacing
  let approx = approxDensity (tail inferedLine) (head inferedLine)
  sampledLine <- evalRandIO (sampleDensity program SampleInfo {start = LinearSpacing.start spacing, stepWidth = LinearSpacing.stepWidth spacing, numberOfSamples})
  let sampledBars = map (\(x, y) -> (x + 0.5 * LinearSpacing.stepWidth spacing, [y])) sampledLine

  toFile def filename $ do
    layout_title .= "PDF"
    setColors [opaque blue, opaque red]
    plot $ plotBars <$> normalBars ["Sampled"] sampledBars
    plot (line ("Inferred ~Area: " ++ showFloatN approx 5) [inferedLine])

normalBars :: (PlotValue x, BarsPlotValue y) => [String] -> [(x, [y])] -> EC l (PlotBars x y)
normalBars titles vals = liftEC $ do
  styles <- sequence [fmap mkStyle takeColor | _ <- titles]
  plot_bars_titles .= titles
  plot_bars_values .= vals
  plot_bars_style .= BarsClustered
  plot_bars_spacing .= BarsFixGap 0 0
  plot_bars_item_styles .= styles
  where
    mkStyle c = (solidFillStyle c, Nothing)

approxDensity :: [(Double, Double)] -> (Double, Double) -> Double
approxDensity [] _point = 0.0
approxDensity (x : xs) point = trapezTwoPoints point x + approxDensity xs x

plotMassToFile :: FileName -> Program -> NumberOfSamples -> IO ()
plotMassToFile filename program numberOfSamples = do
  sampledMasses <- evalRandIO (sampleMass program numberOfSamples)
  let sampledPointLine = toBarLayout sampledMasses
  let inferPoints = map (\(x, _) -> (x, snd $ unwrapEither $ inferProgram program (VFloat x))) sampledMasses
  let inferPointLine = toBarLayout inferPoints
  let totalMass = sum . map snd $ inferPoints
  toFile def filename $ do
    layout_title .= "PMF"
    setColors [withOpacity blue 0.8, withOpacity blue 0.8, withOpacity red 0.8, withOpacity red 0.8]
    setShapes [PointShapeCircle, PointShapeCircle, PointShapeCircle]
    plot $ plotBars <$> pointLines ["Sampled"] sampledPointLine
    plot (pointsWithSize "" 3 sampledMasses)
    plot $ plotBars <$> pointLines ["Inferred ∑Mass: " ++ show totalMass] inferPointLine
    plot (pointsWithSize "" 3 inferPoints)

toBarLayout :: [(Double, Double)] -> [(Double, [Double])]
toBarLayout = map (\(x, y) -> (x, [y]))

pointLines :: (PlotValue x, BarsPlotValue y) => [String] -> [(x, [y])] -> EC l (PlotBars x y)
pointLines titles vals = liftEC $ do
  styles <- sequence [fmap mkStyle takeColor | _ <- titles]
  plot_bars_titles .= titles
  plot_bars_values .= vals
  plot_bars_style .= BarsClustered
  plot_bars_spacing .= BarsFixWidth 1
  plot_bars_item_styles .= styles
  where
    mkStyle c = (solidFillStyle c, Nothing)

pointsWithSize :: String -> Double -> [(x, y)] -> EC l (PlotPoints x y)
pointsWithSize title size values = liftEC $ do
  color <- takeColor
  shape <- takeShape
  plot_points_values .= values
  plot_points_title .= title
  plot_points_style . point_color .= color
  plot_points_style . point_shape .= shape
  plot_points_style . point_radius .= size
  plot_points_style . point_border_color .= color
  plot_points_style . point_border_width .= 1

plotCumulativeToFile :: FileName -> Program -> LinearSpacing -> NumberOfSamples -> IO ()
plotCumulativeToFile filename program spacing numberOfSamples = do
  let inferredLine = createCumulativePoints program spacing
  sampledLine <- evalRandIO (sampleCumulative program SampleInfo {start = LinearSpacing.start spacing, stepWidth = LinearSpacing.stepWidth spacing, numberOfSamples})

  toFile def filename $ do
    layout_title .= "CDF"
    setColors [opaque blue, opaque red]
    plot (line "Sampled" [sampledLine])
    plot (line "Inferred" [inferredLine])

createCumulativePoints :: Program -> LinearSpacing -> [Point2D]
createCumulativePoints program spacing = addYValues (convertCumulativeToFunction program) (xValues spacing)