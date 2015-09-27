module TicTacToe.AI.Generator (randomGene) where

import System.Random

import TicTacToe.AI.DNA
import TicTacToe.Board

data GeneEnum = EIfAnd
              | EIfOr
              | ECurrentCellIs
              | ECellIsEmpty
              | ECellIsNotEmpty
              | ECellIsMine
              | ECellIsTheirs
              | ERank deriving (Show, Eq, Enum, Bounded)

allGenes = [(minBound :: GeneEnum) ..]

numGenes = length allGenes

type RandSeq = [(Int, StdGen)]

randomCell :: (CellIndex -> Gene) -> RandSeq -> (Gene, StdGen)
randomCell construct ((cellNum, gen):_) = (construct cellNum, gen)

randomGene :: RandSeq -> (Gene, StdGen)
randomGene ((id, gen):remaining) =
  case toEnum id :: GeneEnum of
    EIfAnd         -> (IfAnd, gen)
    EIfOr          -> (IfOr, gen)
    ECurrentCellIs -> randomCell CurrentCellIs remaining
    _              -> (Rank 0, gen)
