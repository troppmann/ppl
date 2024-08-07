module Main where

import Parser
import Parser2 qualified as P2
import Sample
import Spn

main :: IO ()
main = do
  s <- readFile "test.ppl"
  let tokenStream = P2.parseText s
  print tokenStream
  print "\n"
  let tok2 = P2.rpn [] tokenStream
  print tok2
  let expr = parseText s
  print expr
  print $ sample expr
  print calculateX0TrueGivenX1False

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