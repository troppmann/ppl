{-# LANGUAGE DuplicateRecordFields #-}

module Main where

-- import DistributionSampler

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

main :: IO ()
main = do
  s <- readFile "test.ppl"
  let expr = unwrap $ parseExpr s
  print expr
  sample0 <- sampleExpr expr
  print sample0
  -- sampledDis <- evalRandIO (sampleDistr expr SampleInfo {start = 0, stepWidth = 0.05, numberOfSamples = 100000})
  -- print sampledDis
  -- print $ density sampledDis 2.0
  -- let integral = validateExpr LinearSpacing {start = -10, end = 10, stepWidth = 0.10} expr
  -- print $ "Validate: " ++ show integral
  let value = (VFloat 1.0)
  let prob = interpret expr value
  print ("Test: " <> show value <> " -> " <> show prob)
  print $ "Mean: " <> show (meanExpr expr)

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