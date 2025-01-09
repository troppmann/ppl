{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import ApproximateIntegration
import Chart
import Control.Monad.Random (evalRandIO, replicateM)
import Data.List
import Debug.Extended
import DistributionSampler
import Infer
import MaximumAPosteriori (mle)
import Mean
import Optimizer
import Parser
import Query
import Representation
import Sample
import Spn
import Control.Monad.Random.Class
import Statistics.Distribution.StudentT (studentT)
import Statistics.Distribution

toFloat :: Value -> Double
toFloat (VFloat f) = f
toFloat _ = -1

main :: IO ()
main = do
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

  print "------Program Unopt"
  print program
  let optProgram = optimize program
  print "------Program Optimize"
  print optProgram

  sample <- sampleProgram program
  print "------Sample Unopt"
  print sample
  optSample <- sampleProgram optProgram
  print "------Sample Optimize"
  print optSample

  let query = QAny -- QTuple (QBool NormalMode False)(QTuple (QBool NormalMode True) (QTuple (QBool NormalMode True) (QTuple (QBool NormalMode False) (QTuple (QBool NormalMode True) QAny))))
  -- let inferSample = optSample
  let prob = qInferProgram program query
  print "------Infer Unopt"
  print prob
  let optProb = qInferProgram optProgram query
  print "------Infer Optimize"
  print optProb

  print "------MLE Unopt"
  let maxSample = mle program query
  print maxSample
  print "------MLE Optimize"
  let maxSampleOpt = mle optProgram query
  print maxSampleOpt
  let spacing = LinearSpacing {start = -9, end = 9, stepWidth = 0.01}
  let numberOfSamples = 100000
  -- plotCumulativeToFile "cdf.svg" program spacing numberOfSamples
  -- plotDensityToFile "pdf.svg" optProgram spacing numberOfSamples
  plotMassToFile "pmf.svg" optProgram numberOfSamples

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


calculateX0TrueGivenX1False :: Float
calculateX0TrueGivenX1False = calculate ([1, 0], [0, 1]) spn / calculate ([1, 0], [1, 1]) spn
  where
    spn = createSpn

createSpn :: Spn
createSpn =
  Sum
    [ ( 0.7,
        Product
          [ Sum [(0.6, Leaf $ Index 0), (0.4, Leaf $ NegIndex 0)],
            Sum [(0.3, Leaf $ Index 1), (0.7, Leaf $ NegIndex 1)]
          ]
      ),
      ( 0.3,
        Product
          [ Sum [(0.9, Leaf $ Index 0), (0.1, Leaf $ NegIndex 0)],
            Sum [(0.2, Leaf $ Index 1), (0.8, Leaf $ NegIndex 1)]
          ]
      )
    ]