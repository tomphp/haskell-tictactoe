{-# LANGUAGE DeriveAnyClass #-}

module TicTacToe.Player where

import AppPrelude

data Player = X
            | O
            deriving (Bounded, CycleEnum, Enum, Eq)

instance Show Player where
  show X = "Crosses"
  show O = "Naughts"

switch :: Player -> Player
switch = csucc
