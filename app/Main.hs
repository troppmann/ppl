{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import ApproximateIntegration
import Chart
import Control.Exception (assert)
import Control.Monad.Random.Class
import Data.Colour.SRGB
import Data.List
import Data.Vector qualified as Vector
import Debug.Extended
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy
import Infer
import MaximumAPosteriori (mmap)
import Numeric
import Optimizer
import Parser
import Query
import Representation
import Sample
import Statistics.Distribution
import Statistics.Distribution.Binomial
import Statistics.Distribution.Geometric
import Statistics.Distribution.Lognormal
import Statistics.Distribution.StudentT (studentT)
import Statistics.Sample
import Text.RawString.QQ

main :: IO ()
main = do
  evaluateBinomial
  evaluateSelectivityBinomial
  evaluateGeometric
  evaluateLognormal
  evaluatePareto
  evaluateIndianGpaProblem
-- playground

-- generates the analytical solution, program plots,
-- quantitative comparison statistics for the Binomial Distribution
evaluateBinomial :: IO ()
evaluateBinomial = do
  putStrLn "--- Evaluate binomial distribution ---"
  let analyticalPmf =
        [ generateAnalyticalPmf "Dandelion" (binomial 20 0.5) [0 .. 20],
          generateAnalyticalPmf "ForestGreen" (binomial 20 0.7) [0 .. 20],
          generateAnalyticalPmf "Cerulean" (binomial 15 0.5) [0 .. 20]
        ]
  printAnalyticalCords analyticalPmf
  let analyticalCdf =
        [ generateAnalyticalCdf "Dandelion" (binomial 20 0.5) [0 .. 20],
          generateAnalyticalCdf "ForestGreen" (binomial 20 0.7) [0 .. 20],
          generateAnalyticalCdf "Cerulean" (binomial 15 0.5) [0 .. 20]
        ]
  printAnalyticalCords analyticalCdf
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
  let program = unwrapEither $ parseProgram binomialProgram
  putStrLn "Generate binomial_pmf.svg ..."
  let boundsPmf = BoundsRect 0 20.0 0 0.2
  let dataPmf =
        [ (dandelion, programPmf program [0.5, 20.0] [0 .. 20]),
          (forestGreen, programPmf program [0.7, 20.0] [0 .. 20]),
          (cerulean, programPmf program [0.5, 15.0] [0 .. 20])
        ]
  plotProgramPmfToFile "binomial_pmf.svg" dataPmf boundsPmf
  putStrLn "Generate binomial_cdf.svg ..."
  let boundsCdf = BoundsRect 0 20.0 0 1.0
  let dataCdf =
        [ (dandelion, programMassCdf program [0.5, 20.0] [0 .. 20]),
          (forestGreen, programMassCdf program [0.7, 20.0] [0 .. 20]),
          (cerulean, programMassCdf program [0.5, 15.0] [0 .. 20])
        ]
  plotProgramMassCdfToFile "binomial_cdf.svg" dataCdf boundsCdf
  writeStats "binomial_pmf_stats.csv" analyticalPmf dataPmf
  writeStats "binomial_cdf_stats.csv" analyticalCdf dataCdf
  let binomialSelectiveProgram =
        [r|
factorial n = if n <= 1 then 1 else n * factorial (n-1);
binomialCoefficient n k = (factorial n)/((factorial k) * (factorial (n-k)));
binomialPmf p n k = (binomialCoefficient n k) * (p ** k) * ((1 - p) ** (n - k));
bernoulli p = Uniform < p;
binomialIntern p n k mass = if k <= 0 then 
	0 
else 
	( if bernoulli ((binomialPmf p n k )/mass) then 
		k
	else 
		binomialIntern p n (k-1)  (mass - ((binomialPmf p n k))));
binomialSelective p n = binomialIntern p n n 1.0;
main p n = binomialSelective p n|]
  let programS = unwrapEither $ parseProgram binomialSelectiveProgram
  putStrLn "Generate binomial_selective_pmf.svg ..."
  let dataPmfS =
        [ (dandelion, programPmf programS [0.5, 20.0] [0 .. 20]),
          (forestGreen, programPmf programS [0.7, 20.0] [0 .. 20]),
          (cerulean, programPmf programS [0.5, 15.0] [0 .. 20])
        ]
  plotProgramPmfToFile "binomial_selective_pmf.svg" dataPmfS boundsPmf
  putStrLn "Generate binomial_selective_cdf.svg ..."
  let dataCdfS =
        [ (dandelion, programMassCdf programS [0.5, 20.0] [0 .. 20]),
          (forestGreen, programMassCdf programS [0.7, 20.0] [0 .. 20]),
          (cerulean, programMassCdf programS [0.5, 15.0] [0 .. 20])
        ]
  plotProgramMassCdfToFile "binomial_selective_cdf.svg" dataCdfS boundsCdf
  writeStats "binomial_selective_pmf_stats.csv" analyticalPmf dataPmfS
  writeStats "binomial_selective_cdf_stats.csv" analyticalCdf dataCdfS

-- generates the analytical solution, program plots,
-- quantitative comparison statistics for the Geometric Distribution
evaluateGeometric :: IO ()
evaluateGeometric = do
  putStrLn "--- Evaluate geometric distribution ---"
  let analyticalPmf =
        [ generateAnalyticalPmf "Dandelion" (geometric 0.3) [1 .. 20],
          generateAnalyticalPmf "ForestGreen" (geometric 0.5) [1 .. 20],
          generateAnalyticalPmf "Cerulean" (geometric 0.7) [1 .. 20]
        ]
  printAnalyticalCords analyticalPmf
  let analyticalCdf =
        [ generateAnalyticalCdf "Dandelion" (geometric 0.3) [1 .. 20],
          generateAnalyticalCdf "ForestGreen" (geometric 0.5) [1 .. 20],
          generateAnalyticalCdf "Cerulean" (geometric 0.7) [1 .. 20]
        ]
  printAnalyticalCords analyticalCdf
  let geometricProgram =
        [r|

  bernoulli p = Uniform < p;
  geometric p = if bernoulli p then 1 else 1 + geometric p;
  main p = geometric p
  |]
  let program = unwrapEither $ parseProgram geometricProgram
  putStrLn "Generate geometric_pmf.svg ..."
  let boundsPmf = BoundsRect 0 20.0 0 0.8
  let dataPmf =
        [ (dandelion, programPmf program [0.3] [1 .. 20]),
          (forestGreen, programPmf program [0.5] [1 .. 20]),
          (cerulean, programPmf program [0.7] [1 .. 20])
        ]
  plotProgramPmfToFile "geometric_pmf.svg" dataPmf boundsPmf
  putStrLn "Generate geometric_cdf.svg ..."
  let boundsCdf = BoundsRect 0 20.0 0 1.0
  let dataCdf =
        [ (dandelion, programMassCdf program [0.3] [1 .. 20]),
          (forestGreen, programMassCdf program [0.5] [1 .. 20]),
          (cerulean, programMassCdf program [0.7] [1 .. 20])
        ]
  plotProgramMassCdfToFile "geometric_cdf.svg" dataCdf boundsCdf
  writeStats "geometric_pmf_stats.csv" analyticalPmf dataPmf
  writeStats "geometric_cdf_stats.csv" analyticalCdf dataCdf

-- generates the analytical solution, program plots,
-- quantitative comparison statistics for the Lognormal Distribution
evaluateLognormal :: IO ()
evaluateLognormal = do
  let valueRange = 0.0001 : [0.05, 0.10 .. 10]
  putStrLn "--- Evaluate lognormalDistr distribution ---"
  let analyticalPdf =
        [ generateAnalyticalPdf "Dandelion" (lognormalDistr 0 1) valueRange,
          generateAnalyticalPdf "ForestGreen" (lognormalDistr 0 0.4) valueRange,
          generateAnalyticalPdf "Cerulean" (lognormalDistr 1.5 1) valueRange
        ]
  printAnalyticalCords analyticalPdf
  let analyticalCdf =
        [ generateAnalyticalCdf "Dandelion" (lognormalDistr 0 1) valueRange,
          generateAnalyticalCdf "ForestGreen" (lognormalDistr 0 0.4) valueRange,
          generateAnalyticalCdf "Cerulean" (lognormalDistr 1.5 1) valueRange
        ]
  printAnalyticalCords analyticalCdf
  let lognormalProgram =
        [r|
  normal mean std = std * Normal + mean;
  exp v = 2.718281828 ** v;
  logNormal mean std = exp (normal mean std);
  main mean std = logNormal mean std
|]
  let program = unwrapEither $ parseProgram lognormalProgram
  putStrLn "Generate lognormal_pdf.svg ..."
  let boundsPdf = BoundsRect 0 10.0 0 1.25
  let dataPdf =
        [ (dandelion, programPmf program [0, 1] valueRange),
          (forestGreen, programPmf program [0, 0.4] valueRange),
          (cerulean, programPmf program [1.5, 1] valueRange)
        ]
  putStrLn "Generate lognormal_cdf.svg ..."
  plotProgramPdfToFile "lognormal_pdf.svg" dataPdf boundsPdf
  let boundsCdf = BoundsRect 0 10.0 0 1.0
  let dataCdf =
        [ (dandelion, programMassCdf program [0, 1] valueRange),
          (forestGreen, programMassCdf program [0, 0.4] valueRange),
          (cerulean, programMassCdf program [1.5, 1] valueRange)
        ]
  plotProgramDensityCdfToFile "lognormal_cdf.svg" dataCdf boundsCdf
  writeStats "lognormal_pdf_stats.csv" analyticalPdf dataPdf
  writeStats "lognormal_cdf_stats.csv" analyticalCdf dataCdf

-- generates the analytical solution, program plots,
-- quantitative comparison statistics for the Pareto Distribution
evaluatePareto :: IO ()
evaluatePareto = do
  let valueRange = 0.0001 : [0.05, 0.10 .. 10.0]
  putStrLn "--- Evaluate lognormalDistr distribution ---"
  let analyticalPdf =
        [ generateAnalyticalPdf "Dandelion" (Pareto 2 1) valueRange,
          generateAnalyticalPdf "ForestGreen" (Pareto 2 2) valueRange,
          generateAnalyticalPdf "Cerulean" (Pareto 2 3) valueRange
        ]
  printAnalyticalCords analyticalPdf
  let analyticalCdf =
        [ generateAnalyticalCdf "Dandelion" (Pareto 2 1) valueRange,
          generateAnalyticalCdf "ForestGreen" (Pareto 2 2) valueRange,
          generateAnalyticalCdf "Cerulean" (Pareto 2 3) valueRange
        ]
  printAnalyticalCords analyticalCdf
  let paretoProgram =
        [r|
  pareto xm alpha = xm * ((1 - Uniform) ** (-1 / alpha));
  main xm alpha = pareto xm alpha
|]
  let program = unwrapEither $ parseProgram paretoProgram
  putStrLn "Generate pareto_pdf.svg ..."
  let boundsPdf = BoundsRect 0 10.0 0 1.5
  let dataPdf =
        [ (dandelion, programPmf program [2, 1] valueRange),
          (forestGreen, programPmf program [2, 2] valueRange),
          (cerulean, programPmf program [2, 3] valueRange)
        ]
  putStrLn "Generate pareto_cdf.svg ..."
  plotProgramPdfToFile "pareto_pdf.svg" dataPdf boundsPdf
  let boundsCdf = BoundsRect 0 10.0 0 1.0
  let dataCdf =
        [ (dandelion, programMassCdf program [2, 1] valueRange),
          (forestGreen, programMassCdf program [2, 2] valueRange),
          (cerulean, programMassCdf program [2, 3] valueRange)
        ]
  plotProgramDensityCdfToFile "pareto_cdf.svg" dataCdf boundsCdf
  writeStats "pareto_pdf_stats.csv" analyticalPdf dataPdf
  writeStats "pareto_cdf_stats.csv" analyticalCdf dataCdf

-- evaluates different queries for the Indian GPA problem
evaluateIndianGpaProblem :: IO ()
evaluateIndianGpaProblem = do
  let programIndianGPA =
        [r|
 bernoulli p = Uniform < p;
 india = (0, if bernoulli 0.1 then 10 else Uniform * 10);
 usa = (1, if bernoulli 0.2 then 4 else Uniform * 4);
 main = if bernoulli 0.5 then india else usa
  |]
  putStrLn "IndianGpaProblem"
  putStrLn programIndianGPA
  let program = unwrapEither $ parseProgram programIndianGPA
  let useScore3_5 = QTuple (QFloat NormalMode 1.0) (QFloat NormalMode 3.5)
  checkQuery program "(1.0, 3.5)" useScore3_5 (1, 0.1)
  let marScore3_5 = QTuple QMar (QFloat NormalMode 3.5)
  checkQuery program "(Q_MAR, 3.5)" marScore3_5 (1, 0.145)
  let conUseScore3_5 = QTuple (QFloat Given 1.0) (QFloat NormalMode 3.5)
  checkQuery program "(1, |3.5)" conUseScore3_5 (1, 0.2)
  let mapScore4 = QTuple QAny (QFloat NormalMode 4.0)
  checkMap program "(Q_Query, 4.0)" mapScore4 (0, 0.1) (VTuple (VFloat 1.0) (VFloat 4.0))
  let programScoreIndianGPA =
        [r|

 bernoulli p = Uniform < p;
 india = (0, if bernoulli 0.1 then (True, 10) else (False, Uniform * 10));
 usa = (1, if bernoulli 0.2 then (True, 4) else (False, Uniform * 4));
 main = if bernoulli 0.5 then india else usa
  |]
  let secondProgram = unwrapEither $ parseProgram programScoreIndianGPA
  let perfectScore = QTuple QMar (QTuple (QBool NormalMode True) QMar)
  checkQuery secondProgram "(Q_MAR, True, Q_MAR)" perfectScore (0, 0.15)
  let mapPerfectScore = QTuple QMar (QTuple QAny QMar)
  checkMap secondProgram "(Q_MAR, Q_Query, Q_MAR)" mapPerfectScore (0, 0.45) (VBool False)

-- evaluates selective and non-selective Binomial distribution programs with MAP queries
evaluateSelectivityBinomial :: IO ()
evaluateSelectivityBinomial = do
  putStrLn "Selectivity"
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
    main = binomial 0.7 20 
    ;|]
  let nonSelectiveProgram = unwrapEither $ parseProgram binomialProgram
  checkMap nonSelectiveProgram "(Q_Query)" QAny (0, 0.7 ** 20) (VFloat 20)
  let binomialSelectiveProgram =
        [r|
factorial n = if n <= 1 then 1 else n * factorial (n-1);
binomialCoefficient n k = (factorial n)/((factorial k) * (factorial (n-k)));
binomialPmf p n k = (binomialCoefficient n k) * (p ** k) * ((1 - p) ** (n - k));
bernoulli p = Uniform < p;
binomialIntern p n k mass = if k <= 0 then 
	0 
else 
	( if bernoulli ((binomialPmf p n k )/mass) then 
		k
	else 
		binomialIntern p n (k-1)  (mass - ((binomialPmf p n k))));
binomialSelective p n = binomialIntern p n n 1.0;
main = binomialSelective 0.7 20
|]
  let selectiveProgram = unwrapEither $ parseProgram binomialSelectiveProgram
  checkMap selectiveProgram "(Q_Query)" QAny (0, 0.191638) (VFloat 14)

--
-- Helper functions
--

checkQuery :: Program -> String -> QueryType -> (Dimension, Probability) -> IO ()
checkQuery program queryString query expected = do
  putStrLn $ "assert Query " ++ queryString ++ " == " ++ show expected ++ " => " ++ show (assert result result)
  where
    dimProb = dbg $ unwrapEither $ qInferProgram program query
    result = checkEqDimProb dimProb expected

checkMap :: Program -> String -> QueryType -> (Dimension, Probability) -> Value -> IO ()
checkMap program queryString query expected expValue = do
  putStrLn $ "assert Map Query " ++ queryString ++ " == " ++ show expected ++ " => " ++ show (assert resultDimProb resultDimProb)
  putStrLn $ "       with expected value " ++ show expValue ++ " == " ++ show trimmed ++ " => " ++ show (assert resultValue resultValue)
  where
    (dimProb, value) = unwrapEither $ mmap program query
    resultDimProb = checkEqDimProb dimProb expected
    trimmed = removeVmar value
    resultValue = trimmed == expValue

removeVmar :: Value -> Value
removeVmar (VTuple v1 v2@(VFloat f2)) = if isNaN f2 then removeVmar v1 else VTuple (removeVmar v1) v2
removeVmar (VTuple v1@(VFloat f1) v2) = if isNaN f1 then removeVmar v2 else VTuple v1 (removeVmar v2)
removeVmar (VTuple v1 v2) = VTuple (removeVmar v1) (removeVmar v2)
removeVmar v = v

defaultErrorMargin :: Double
defaultErrorMargin = 0.00001

checkEqDimProb :: (Dimension, Probability) -> (Dimension, Probability) -> Bool
checkEqDimProb (dimValue, probValue) (dimExpected, probExpected)
  | dimValue /= dimExpected = False
  | abs (probExpected - probValue) > defaultErrorMargin = False
  | otherwise = True

writeStats :: String -> [(a1, [(Double, Double)])] -> [(a2, [(Double, Double)])] -> IO ()
writeStats filename analytical sampled = do
  let pdfDiffs = zipWith (\l1 l2 -> calculateDiff (snd l1) (snd l2)) analytical sampled
  let diffTablePdf = generateDiffTable $ map calcStats pdfDiffs
  putStrLn $ "Generate " ++ filename ++ " ..."
  writeFile filename diffTablePdf
  putStrLn diffTablePdf

calculateDiff :: [(Double, Double)] -> [(Double, Double)] -> [Double]
calculateDiff = zipWith (curry diff)
  where
    diff ((x0, y0), (x1, y1)) = assert (x0 == x1) (abs (y1 - y0))

calcStats :: [Double] -> Stats
calcStats diffs = Stats {samples, minD, maxD, meanD, medianD, stdD, varianceD, totalD}
  where
    minD = minimum diffs
    samples = length diffs
    maxD = maximum diffs
    (meanD, varianceD) = meanVarianceUnb $ Vector.fromList diffs
    stdD = sqrt varianceD
    medianD = median diffs
    totalD = sum diffs

median :: (Ord a, Fractional a) => [a] -> a
median xs =
  if odd len
    then sorted !! half
    else ((sorted !! (half - 1)) + (sorted !! half)) * 0.5
  where
    len = length xs
    half = len `div` 2
    sorted = sort xs

data Stats = Stats {samples :: Int, minD :: Double, maxD :: Double, medianD :: Double, meanD :: Double, stdD :: Double, varianceD :: Double, totalD :: Double} deriving (Show)

generateDiffTable :: [Stats] -> String
generateDiffTable stats = content
  where
    header = "Samples, Min, Max, Median, Mean, Std, Total"
    entry s = intercalate " & " $ map (\x -> "\\num{" ++ x ++ "}") $ show (samples s) : map show [minD s, maxD s, medianD s, meanD s, stdD s, totalD s]
    content = unlines $ header : map entry stats

data Pareto = Pareto {xm :: Double, alpha :: Double} deriving (Show)

instance Distribution Pareto where
  cumulative pareto x = if x > xm pareto then 1 - ((xm pareto / x) ** alpha pareto) else 0

instance ContDistr Pareto where
  density pareto x = if x > xm pareto then alpha pareto * (xm pareto ** alpha pareto) / (x ** (alpha pareto + 1)) else 0
  quantile = undefined

formatFloatN :: (RealFloat a) => a -> Int -> String
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

formatFloatE :: (RealFloat a) => a -> Int -> String
formatFloatE floatNum numOfDecimals = showEFloat (Just numOfDecimals) floatNum ""

showF :: (RealFloat a) => a -> String
showF float = formatFloatN float 10

showE :: (RealFloat a) => a -> String
showE float = formatFloatE float 4

generateAnalyticalPmf :: (Show d, DiscreteDistr d) => String -> d -> [Int] -> (String, [(Double, Double)])
generateAnalyticalPmf colorName distr values = (indicator, generatePmf distr values)
  where
    indicator = "PMF :: " ++ colorName ++ " :: " ++ show distr

generatePmf :: (DiscreteDistr d) => d -> [Int] -> [(Double, Double)]
generatePmf distribution = map (\x -> (fromIntegral x, probability distribution x))

generateAnalyticalCdf :: (Show d, Distribution d) => String -> d -> [Double] -> (String, [(Double, Double)])
generateAnalyticalCdf colorName distr values = (indicator, generateCdf distr values)
  where
    indicator = "CDF :: " ++ colorName ++ " :: " ++ show distr

generateCdf :: (Distribution d) => d -> [Double] -> [(Double, Double)]
generateCdf distribution = map (\x -> (x, cumulative distribution x))

generatePdf :: (ContDistr d) => d -> [Double] -> [(Double, Double)]
generatePdf distribution = map (\x -> (x, density distribution x))

generateAnalyticalPdf :: (Show d, ContDistr d) => String -> d -> [Double] -> (String, [(Double, Double)])
generateAnalyticalPdf colorName distr values = (indicator, generatePdf distr values)
  where
    indicator = "PDF :: " ++ colorName ++ " :: " ++ show distr

printAnalyticalCords :: [(String, [(Double, Double)])] -> IO ()
printAnalyticalCords = mapM_ printPmf

printPmf :: (String, [(Double, Double)]) -> IO ()
printPmf (indicator, cords) = do
  putStrLn indicator
  putStrLn $ intercalate "" $ map (\(x, y) -> "(" ++ showF x ++ "," ++ showF y ++ ")") cords

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

--
-- Plot Generation
--
dandelion :: Colour Double
dandelion = sRGB24 250 180 44

forestGreen :: Colour Double
forestGreen = sRGB24 20 152 80

cerulean :: Colour Double
cerulean = sRGB24 20 163 230

data BoundsRect = BoundsRect
  { xMin :: Double,
    xMax :: Double,
    yMin :: Double,
    yMax :: Double
  }
  deriving (Show)

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

plotProgramPdfToFile :: String -> [(Colour Double, [(Double, Double)])] -> BoundsRect -> IO ()
plotProgramPdfToFile filename plotData rect = do
  let fileOptions = FileOptions (160, 126) SVG loadSansSerifFonts
  toFile fileOptions filename $ do
    layout_margin .= 2
    layout_y_axis . laxis_generate .= scaledAxis def (yMin rect, yMax rect)
    layout_y_axis . laxis_title .= "PDF"
    layout_y_axis . laxis_style . axis_label_gap .= 4
    layout_x_axis . laxis_generate .= scaledAxis def (xMin rect, xMax rect)
    layout_x_axis . laxis_style . axis_label_gap .= 1
    layout_x_axis . laxis_title .= "x"
    setColors $ map (opaque . fst) plotData
    let cords = map snd plotData
    mapM_ (plot . plotLine "" . (: [])) cords

plotProgramDensityCdfToFile :: String -> [(Colour Double, [(Double, Double)])] -> BoundsRect -> IO ()
plotProgramDensityCdfToFile filename plotData rect = do
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
    mapM_ ((plot . plotLine "") . (: []) . snd) plotData

pointsWithSize :: String -> Double -> [(x, y)] -> EC l (PlotPoints x y)
pointsWithSize title size values = liftEC $ do
  color <- takeColor
  shape <- takeShape
  plot_points_values .= values
  plot_points_title .= title
  plot_points_style . point_color .= color
  plot_points_style . point_shape .= shape
  plot_points_style . point_radius .= size
  plot_points_style . point_border_color .= opaque black
  plot_points_style . point_border_width .= 0.2

plotLine :: String -> [[(x, y)]] -> EC l (PlotLines x y)
plotLine title values = liftEC $ do
  color <- takeColor
  plot_lines_title .= title
  plot_lines_values .= values
  plot_lines_style . line_color .= color

-- This is a little playground to test the framework.
-- Remember to have fun.
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
  -- let maxSample = mmap program query
  -- print maxSample
  print "------MMAP Optimize"
  -- let maxSampleOpt = mmap optProgram query
  -- print maxSampleOpt
  let spacing = LinearSpacing {start = -10, end = 10, stepWidth = 0.01}
  let numberOfSamples = 100000
  plotCumulativeToFile "cdf.svg" program spacing numberOfSamples
  plotDensityToFile "pdf.svg" optProgram spacing numberOfSamples
  -- plotMassToFile "pmf.svg" optProgram numberOfSamples
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
