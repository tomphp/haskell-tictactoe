module TicTacToe.Line
  ( Line(..)
  , winner
  ) where

import Control.Error.Safe (headZ)
import Data.List (nub)

newtype Line a = Line [Maybe a] deriving (Eq, Show)

winner :: Eq a => Line a -> Maybe a
winner (Line cs)
  | uniqueCells /= 1 = Nothing
  | otherwise        = join $ headZ cs
  where uniqueCells = length $ nub cs
