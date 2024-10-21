module Shorter
  ( shorter,
  )
where

import Representation

class Shorter a where
  shorter :: a -> String

instance Shorter String where
  shorter string = shorterStringWithCount 50 string

shorterStringWithCount :: Int -> String -> String
shorterStringWithCount _ [] = []
shorterStringWithCount 0 (x : xs) = ".. "
shorterStringWithCount count (x : xs) = x : shorterStringWithCount (count - 1) xs

instance Shorter Value where
  shorter (VFloat float) = show float
  shorter (VBool bool) = show bool
  shorter (VTuple v1 v2) = "(" <> shorter v1 <> "," <> shorter v2 <> ")"