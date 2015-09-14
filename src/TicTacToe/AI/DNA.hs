module TicTacToe.AI.DNA where

import TicTacToe.Board

data Gene = IfAnd | IfOr |
            CurrentCellIs CellIndex |
            CellIsEmpty CellIndex | CellIsNotEmpty CellIndex |
            CellIsMine CellIndex | CellIsTheirs CellIndex |
            Rank Int deriving (Eq, Show)

type DNA = [Gene]

isCondition :: Gene -> Bool
isCondition gene = case gene of
  CurrentCellIs _  -> True
  CellIsEmpty _    -> True
  CellIsNotEmpty _ -> True
  CellIsMine _     -> True
  CellIsTheirs _   -> True
  _                -> False

isStatement :: Gene -> Bool
isStatement gene = case gene of
  IfAnd  -> True
  IfOr   -> True
  Rank _ -> True
  _      -> False

findGeneType :: DNA -> (Gene -> Bool) -> Maybe (Gene, DNA)
findGeneType dna matchFunc = case dna' of
  x:xs -> Just (x, xs)
  []   -> Nothing
  where dna' = dropWhile (not . matchFunc) dna

