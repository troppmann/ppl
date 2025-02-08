{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Chart
import Control.Monad.Random.Class
import Data.Colour.SRGB
import Data.List
import Debug.Extended
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Infer
import Numeric
import Optimizer
import Parser
import Query
import Representation
import Sample
import Statistics.Distribution
import Statistics.Distribution.Binomial
import Statistics.Distribution.Geometric
import Statistics.Distribution.StudentT (studentT)
import Text.RawString.QQ
import Graphics.Rendering.Chart.Easy

main :: IO ()
main = do
  -- evaluateBinomial
  -- evaluateGeometric
  putStrLn "Done"

evaluateBinomial :: IO ()
evaluateBinomial = do
  putStrLn "--- Evaluate binomial distribution ---"
  putStrLn "PMF binomial"
  printPmf "Dandelion" (binomial 20 0.5) [0 .. 20]
  printPmf "ForestGreen" (binomial 20 0.7) [0 .. 20]
  printPmf "Cerulean" (binomial 15 0.5) [0 .. 20]
  putStrLn "CDF binomial"
  printCdf "Dandelion" (binomial 20 0.5) [0 .. 20]
  printCdf "ForestGreen" (binomial 20 0.7) [0 .. 20]
  printCdf "Cerulean" (binomial 15 0.5) [0 .. 20]
  let binomialProgram =
        [r|

  binomial p n = if n <= 0 then
        0
                else (
        if Uniform < p then
            1 + binomial p (n-1)
        else
            binomial p (n-1)
    );
  main p n = binomial p n|]
  putStrLn "Generate binomial_pmf.svg ..."
  let bounds = BoundsRect 0 20.0 0 0.2
  let program = unwrapEither $ parseProgram binomialProgram
  let dataPmf =
        [ (dandelion, programPmf program [0.5, 20.0] [0 .. 20]),
          (forestGreen, programPmf program [0.7, 20.0] [0 .. 20]),
          (cerulean, programPmf program [0.5, 15.0] [0 .. 20])
        ]
  plotProgramPmfToFile "binomial_pmf.svg" dataPmf bounds
  putStrLn "Generate binomial_cdf.svg ..."
  let bounds = BoundsRect 0 20.0 0 1.0
  let dataCdf =
        [ (dandelion, programMassCdf program [0.5, 20.0] [0 .. 20]),
          (forestGreen, programMassCdf program [0.7, 20.0] [0 .. 20]),
          (cerulean, programMassCdf program [0.5, 15.0] [0 .. 20])
        ]
  plotProgramMassCdfToFile "binomial_cdf.svg" dataCdf bounds
  putStrLn "Done"

evaluateGeometric :: IO ()
evaluateGeometric = do
  putStrLn "--- Evaluate geometric distribution ---"
  putStrLn "PMF geometric"
  printPmf "Dandelion" (geometric 0.3) [1 .. 20]
  printPmf "ForestGreen" (geometric 0.5) [1 .. 20]
  printPmf "Cerulean" (geometric 0.7) [1 .. 20]
  putStrLn "CDF geometric"
  printCdf "Dandelion" (geometric 0.3) [1 .. 20]
  printCdf "ForestGreen" (geometric 0.5) [1 .. 20]
  printCdf "Cerulean" (geometric 0.7) [1 .. 20]

formatFloatN :: (RealFloat a) => a -> Int -> String
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

showF :: (RealFloat a) => a -> String
showF float = formatFloatN float 20

generatePmf :: (DiscreteDistr d) => d -> [Int] -> [Char]
generatePmf distribution listOfNumbers = intercalate "" (map ((\(x, y) -> "(" ++ show x ++ "," ++ showF y ++ ")") . (\x -> (x, probability distribution x))) listOfNumbers)

printPmf :: (Show d, DiscreteDistr d) => String -> d -> [Int] -> IO ()
printPmf colorString distribution listOfNumbers = do
  putStrLn $ "PMF :: " ++ colorString ++ " :: " ++ show distribution
  putStrLn $ generatePmf distribution listOfNumbers

generateCdf :: (Distribution d) => d -> [Double] -> [Char]
generateCdf distribution listOfNumbers = intercalate "" (map ((\(x, y) -> "(" ++ show x ++ "," ++ showF y ++ ")") . (\x -> (x, cumulative distribution x))) listOfNumbers)

printCdf :: (Show d, Distribution d) => String -> d -> [Double] -> IO ()
printCdf colorString distribution listOfNumbers = do
  putStrLn $ "CDF :: " ++ colorString ++ " :: " ++ show distribution
  putStrLn $ generateCdf distribution listOfNumbers

programPmf :: Program -> [Double] -> [Double] -> [(Double, Probability)]
programPmf program doubleArgs = map (\x -> (x, prob x))
  where
    prob x = snd $ unwrapEither $ inferProgramArgs program args (VFloat x)
    args = map (Representation.Const . VFloat) doubleArgs

programMassCdf :: Program -> [Double] -> [Double] -> [(Double, Probability)]
programMassCdf program doubleArgs = map (\x -> (x, prob x))
  where
    prob x = snd $ unwrapEither $ qInferProgramArgs program args (QLe NormalMode x)
    args = map (Representation.Const . VFloat) doubleArgs
dandelion :: Colour Double
dandelion = sRGB24 250 180 44

forestGreen :: Colour Double
forestGreen = sRGB24 20 152 80

cerulean :: Colour Double
cerulean = sRGB24 20 163 230

data BoundsRect = BoundsRect {
  xMin :: Double,
  xMax :: Double,
  yMin :: Double,
  yMax :: Double
} deriving (Show)

plotProgramPmfToFile :: String -> [(Colour Double, [(Double, Double)])] -> BoundsRect -> IO ()
plotProgramPmfToFile filename plotData rect = do
  let fileOptions = FileOptions (160, 126) SVG loadSansSerifFonts
  toFile fileOptions filename $ do
    layout_margin .= 2
    layout_y_axis . laxis_generate .= scaledAxis def (yMin rect, yMax rect)
    layout_y_axis . laxis_title .= "PMF"
    layout_y_axis . laxis_style . axis_label_gap .= 4
    layout_x_axis . laxis_generate .= scaledAxis def (xMin rect, xMax rect)
    layout_x_axis . laxis_style . axis_label_gap .= 1
    layout_x_axis . laxis_title .= "x"
    setColors $ map (opaque . fst) plotData
    setShapes $ repeat PointShapeCircle
    mapM_ ((plot . pointsWithSize "" 2) . snd) plotData

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

plotProgramMassCdfToFile :: String -> [(Colour Double, [(Double, Double)])] -> BoundsRect -> IO ()
plotProgramMassCdfToFile filename plotData rect = do
  let fileOptions = FileOptions (160, 126) SVG loadSansSerifFonts
  toFile fileOptions filename $ do
    layout_margin .= 2
    layout_y_axis . laxis_generate .= scaledAxis def (yMin rect, yMax rect)
    layout_y_axis . laxis_title .= "CDF"
    layout_y_axis . laxis_style . axis_label_gap .= 4
    layout_x_axis . laxis_generate .= scaledAxis def (xMin rect, xMax rect)
    layout_x_axis . laxis_style . axis_label_gap .= 1
    layout_x_axis . laxis_title .= "x"
    setColors $ map (opaque . fst) plotData
    setShapes $ repeat PointShapeCircle
    mapM_ ((plot . pointsWithSize "" 2) . snd) plotData

playground :: IO ()
playground = do
  s <- readFile "test.ppl"
  let parseOptions = ParseOptions {optimization = False, maxLoopUnroll = 0}
  let programOld = unwrapEither $ parseProgramWithOptions parseOptions s
  let program =
        addCustomFnToProgram
          "studentT"
          inferStudentT
          cumulativeStudentT
          sampleStudentT
          programOld
  -- let program = wrapInMain $ unwrapEither $ parseExpr "if Uniform < 0.5 then (if Uniform < 0.01 then 4 else Uniform * 4) else (if Uniform < 0.01 then 10 else Uniform * 10)"

  print "------Program Unopt"
  print program
  let optProgram = optimize program
  print "------Program Optimize"
  -- print optProgram

  sample <- sampleProgram program
  print "------Sample Unopt"
  print sample
  optSample <- sampleProgram optProgram
  print "------Sample Optimize"
  print optSample
  let query = (QAny) -- QTuple (QFloat Given 4.0) (QTuple (QBool NormalMode True) (QFloat Given 3.0)) -- QTuple (QBool NormalMode False)(QTuple (QBool NormalMode True) (QTuple (QBool NormalMode True) (QTuple (QBool NormalMode False) (QTuple (QBool NormalMode True) QAny))))
  -- let inferSample = optSample
  let prob = qInferProgram program query
  print "------Infer Unopt"
  print prob
  let optProb = qInferProgram optProgram query
  print "------Infer Optimize"
  print optProb

  print "------MMAP Unopt"
  --let maxSample = mmap program query
  -- print maxSample
  print "------MMAP Optimize"
  --let maxSampleOpt = mmap optProgram query
  -- print maxSampleOpt
  --let spacing = LinearSpacing {start = -10, end = 10, stepWidth = 0.01}
  let numberOfSamples = 100000
  -- plotCumulativeToFile "cdf.svg" program spacing numberOfSamples
  -- plotDensityToFile "pdf.svg" optProgram spacing numberOfSamples
  plotMassToFile "pmf.svg" optProgram numberOfSamples
  print ""

-- let program = [("main", FnCall "dice" [Const $ VFloat 6.0]),("dice", IfThenElse (LessThanOrEqual (FnParameter 0) (Const $ VFloat 1.0)) (FnParameter 0) (IfThenElse (LessThan Uniform (Divide (Const $ VFloat 1.0) (FnParameter 0))) (FnParameter 0) (FnCall "dice" [Subtract (FnParameter 0) (Const $ VFloat 1.0)])))]
-- print program
-- sample <- sampleProgram program
-- print sample
-- sampledDis <- evalRandIO (sampleDistr expr SampleInfo {start = 0, stepWidth = 0.05, numberOfSamples = 100000})
-- print sampledDis
-- print $ getDensity sampledDis 2.0
-- let integral = validateExpr LinearSpacing {start = -10, end = 10, stepWidth = 0.10} expr
-- print $ "Validate: " ++ show integral
-- let spacing = LinearSpacing {start = -4, end = 60, stepWidth = 0.1}
-- let numberOfSamples = 100000
-- plotDensityToFile "pdf.svg" program spacing numberOfSamples
-- plotMassToFile "pmf.svg" program numberOfSamples
-- let value = VFloat 0.0
-- let prob = infer expr value
-- print ("Test: " <> show value <> " -> " <> showFloatN (snd $ unwrapEither prob) 5)
-- print $ "Mean: " <> show (meanExpr expr)

sampleStudentT :: (MonadRandom m) => m Value
sampleStudentT = do
  rValue <- getRandomR (0, 1)
  let normal = studentT 1.0
  let nValue = quantile normal rValue
  return $ VFloat nValue

inferStudentT :: Value -> DimensionalProbability
inferStudentT (VFloat v) = (1, density distr v)
  where
    distr = studentT 1.0
inferStudentT _ = (0, 0.0)

cumulativeStudentT :: Double -> Probability
cumulativeStudentT = cumulative distr
  where
    distr = studentT 1.0
