module Prelude
  ( module ClassyPrelude 
  , CycleEnum(..)
  ) where

import ClassyPrelude hiding (catchIO)

class (Bounded a, Enum a, Eq a) => CycleEnum a where
  cpred :: a -> a
  cpred x
    | x == minBound = maxBound
    | otherwise     = pred x

  csucc :: a -> a
  csucc x
    | x == maxBound = minBound
    | otherwise     = succ x
