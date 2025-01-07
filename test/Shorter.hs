module Shorter
  ( shorter,
  )
where

import Query
import Representation

class Shorter a where
  shorter :: a -> String

instance Shorter String where
  shorter = shorterStringWithCount 50

shorterStringWithCount :: Int -> String -> String
shorterStringWithCount _ [] = []
shorterStringWithCount 0 (x : xs) = ".. "
shorterStringWithCount count (x : xs) = x : shorterStringWithCount (count - 1) xs

instance Shorter Value where
  shorter (VFloat float) = show float
  shorter (VBool bool) = show bool
  shorter (VTuple v1 v2) = "(" <> shorter v1 <> "," <> shorter v2 <> ")"

instance Shorter QueryType where
  shorter QAny = "_"
  shorter (QBool _ bool) = show bool
  shorter (QFloat _ float) = show float
  shorter (QLt _ float) = "_ < " ++ show float
  shorter (QLe _ float) = "_ <= " ++ show float
  shorter (QGt _ float) = "_ > " ++ show float
  shorter (QGe _ float) = "_ >= " ++ show float
  shorter (QTuple v1 v2) = "(" <> shorter v1 <> "," <> shorter v2 <> ")"
