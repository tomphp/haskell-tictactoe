{-# LANGUAGE DeriveAnyClass #-}

module TicTacToe.Player where

data Player = Naughts | Crosses deriving (Bounded, CycleEnum, Enum, Eq, Show)

switch :: Player -> Player
switch = csucc
