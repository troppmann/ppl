{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import ApproximateIntegration
import Debug.Extended
import DistributionSampler
import Interpret
import Mean
import Parser.Expr
import Query
import Representation
import Sample
import Spn
import Chart
import Control.Monad.Random (replicateM, evalRandIO)
import Data.List
import Representation (Expr(IfThenElse))
import Optimizer

toFloat :: Value -> Double
toFloat (VFloat f) = f
toFloat _ = -1

main :: IO ()
main = do
  --s <- readFile "test.ppl"
  --let program = wrapInMain $ unwrapEither $ parseExpr s
  print "Programs"
  --let program = [("main", FnCall "factorial" [Const $ VFloat 5.0]),("factorial", IfThenElse (LessThanOrEqual (FnParameter 0) (Const $ VFloat 1.0)) (Const $ VFloat 1.0) (Multiply (FnParameter 0) (FnCall "factorial" [Subtract (FnParameter 0)(Const $ VFloat 1.0)])))]
  let program = [("main", FnCall "dice" [Const $ VFloat 6.0]),("dice", IfThenElse (LessThanOrEqual (FnParameter 0) (Const $ VFloat 1.0)) (FnParameter 0) (IfThenElse (LessThan Uniform (Divide (Const $ VFloat 1.0) (FnParameter 0))) (FnParameter 0) (FnCall "dice" [Subtract (FnParameter 0) (Const $ VFloat 1.0)])))]
  print program
  let optProgram = optimize program
  print $ head optProgram
  print "Samples"
  sample <- sampleProgram program
  print sample
  optSample <- sampleProgram optProgram
  print optSample
  --let program = [("main", FnCall "dice" [Const $ VFloat 6.0]),("dice", IfThenElse (LessThanOrEqual (FnParameter 0) (Const $ VFloat 1.0)) (FnParameter 0) (IfThenElse (LessThan Uniform (Divide (Const $ VFloat 1.0) (FnParameter 0))) (FnParameter 0) (FnCall "dice" [Subtract (FnParameter 0) (Const $ VFloat 1.0)])))]
  --print program
  -- sample <- sampleProgram program
  -- print sample
  -- sampledDis <- evalRandIO (sampleDistr expr SampleInfo {start = 0, stepWidth = 0.05, numberOfSamples = 100000})
  -- print sampledDis
  -- print $ density sampledDis 2.0
  -- let integral = validateExpr LinearSpacing {start = -10, end = 10, stepWidth = 0.10} expr
  -- print $ "Validate: " ++ show integral
  --let spacing = LinearSpacing {start = -4, end = 60, stepWidth = 0.1}
  --let numberOfSamples = 100000
  --plotDensityToFile "pdf.svg" program spacing numberOfSamples
  --plotMassToFile "pmf.svg" program numberOfSamples
  --let value = VFloat 0.0
  --let prob = interpret expr value
  --print ("Test: " <> show value <> " -> " <> showFloatN (snd $ unwrapEither prob) 5)
  -- print $ "Mean: " <> show (meanExpr expr)

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