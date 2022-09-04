module TicTacToe.Line
  ( Line(..)
  , winner
  ) where

import AppPrelude

import Data.List (nub)

newtype Line a = Line [Maybe a] deriving (Eq, Show)

winner :: Eq a => Line a -> Maybe a
winner (Line cells) =
  case nub cells of
    [cell] -> cell
    _      -> Nothing
