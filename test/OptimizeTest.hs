module OptimizeTest (tests) where

import Assert
import Debug.Extended
import Parser
import Problems
import Query
import Representation
import Shorter
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Data.List

testOptimize :: String -> String -> TestTree
testOptimize programString optProgramString = testCase testString $ do
  let optOptimize = ParseOptions True 15
  optProgram <-  assertRight $ parseProgramWithOptions optOptimize programString
  let optNoOptimize = ParseOptions False 0
  program <-  assertRight $ parseProgramWithOptions optNoOptimize optProgramString
  let optExpr = unwrapMaybe $ lookup "main" $ optProgram
  let expected = unwrapMaybe $ lookup "main" $ program
  optExpr @?= expected
  where
    testString = shorter programString <> ":Opt " <> shorter optProgramString
tests =
  testGroup
    "Optimize"
    [ testOptimize "main = 3 + 4" "main = 7"
      , testOptimize "main = 3 - 4" "main = -1"
      , testOptimize "main = Uniform * 0.0" "main = 0"
      , testOptimize "main = Uniform * 1.0" "main = Uniform"
      , testOptimize "main = 1 * Uniform + (3 + 7)" "main = Uniform + 10"
      , testOptimize "main = 3 * 1 * Uniform + (3 + 7)" "main = 3 * Uniform + 10"
      , testOptimize "main = 1 * Uniform + 3 + 7" "main = Uniform + 10"
      , testOptimize "main = if 3 < 7 then Uniform else 8" "main = Uniform"
      , testOptimize "main = (3 < 4) && (Uniform < 0.5)" "main = Uniform < 0.5"
      , testOptimize "main = (Uniform < 0.5) && (3 < 4)" "main = Uniform < 0.5"
      , testOptimize "main = (Uniform < 0.5) && (1 > 20)" "main = False"
      , testOptimize "main = (Uniform < 0.5) || (3 < 4)" "main = True"
      , testOptimize "main = (3 < 4) || (Uniform < 0.5)" "main = True"
      , testOptimize "main = (1 > 20) || (Uniform < 0.5)" "main = Uniform < 0.5"
      , testOptimize "main = linear 2 1 4;linear m x b= m*x+b" "main = 6"
      , testOptimize "main = (Uniform * 3) * 3" "main = Uniform * 9"
      , testOptimize "main = 3 * (Uniform * 3) * (1 + 2) * 3" "main = Uniform * 81"
      , testOptimize "main = 3 + (Uniform + 3) + (1 * 2) + 3" "main = Uniform + 11"
      , testOptimize "main = (9 * Uniform + 3) * 10" "main = Uniform * 90 + 30 "
      , testOptimize "main = (((9 * Uniform) * 10) + (3 * 10)) + 10" "main = Uniform * 90 + 40"
      , testOptimize "main = (10 + ((((4 + 5) * Uniform) * 10) + (3 * 10))) * 2" "main = Uniform * 180 + 80"
      , testOptimize "main = (10 + (((Uniform * (4 + 5)) * 10) + (3 * 10))) * 2" "main = Uniform * 180 + 80"
      , testOptimize "main = (Uniform + 3) * 10" "main = Uniform * 10 + 30 "
      , testOptimize "main = ((Uniform * 10) + (3 * 10))" "main = Uniform * 10 + 30 "
      , testOptimize "main = 4 ** Uniform" "main = 4 ** Uniform"
      , testOptimize "main = Uniform ** 4" "main = Uniform ** 4"
    ]
