module Spn
  ( Spn (..),
    Indicator (..),
    calculate,
    scope,
    weightsRepresentProbability,
    complete,
    valid,
  )
where

import Data.Set qualified as Set

type Weight = Float

data Indicator = Index Int | NegIndex Int deriving (Eq, Show, Read)

data Spn = Leaf Indicator | Sum [(Weight, Spn)] | Product [Spn] deriving (Eq, Show, Read)

type Probability = Float

calculate :: ([Float], [Float]) -> Spn -> Probability
calculate input (Leaf (Index i)) = fst input !! i
calculate input (Leaf (NegIndex i)) = snd input !! i
calculate input (Sum xs) = sum $ map (\(weight, node) -> weight * calculate input node) xs
calculate input (Product xs) = product $ map (calculate input) xs

valid :: Spn -> Bool
valid spn = decomposable spn && complete spn

scope :: Spn -> Set.Set Int
scope (Leaf (Index i)) = Set.singleton i
scope (Leaf (NegIndex i)) = Set.singleton i
scope (Sum xs) = Set.unions $ map (scope . snd) xs
scope (Product xs) = Set.unions $ map scope xs

weightsRepresentProbability :: Spn -> Bool
weightsRepresentProbability (Leaf _) = True
weightsRepresentProbability (Sum xs) = sum (map fst xs) == 1 && all (weightsRepresentProbability . snd) xs
weightsRepresentProbability (Product xs) = all weightsRepresentProbability xs

complete :: Spn -> Bool
complete (Leaf _) = True
complete (Sum xs) = allEqual (map (scope . snd) xs) && all (complete . snd) xs
complete (Product xs) = all complete xs

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

decomposable :: Spn -> Bool
decomposable (Leaf _) = True
decomposable (Sum xs) = all (decomposable . snd) xs
decomposable (Product xs) = allDisjoint (map scope xs) && all decomposable xs

allDisjoint :: (Ord a) => [Set.Set a] -> Bool
allDisjoint [] = True
allDisjoint (x : xs) = all (\y -> null (x `Set.intersection` y)) xs && allDisjoint xs