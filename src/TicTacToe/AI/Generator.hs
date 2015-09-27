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

randomCell :: (CellIndex -> Gene) -> StdGen -> (Gene, StdGen)
randomCell construct gen = (construct cellNum, newGen)
  where (cellNum, newGen) = randomR (0, 8) gen

constructGene :: Int -> StdGen -> (Gene, StdGen)
constructGene id gen = case toEnum id :: GeneEnum of
  EIfAnd         -> (IfAnd, gen)
  EIfOr          -> (IfOr, gen)
  ECurrentCellIs -> randomCell CurrentCellIs gen
  _              -> (Rank 0, gen)


randomGene :: StdGen -> (Gene, StdGen)
randomGene gen = constructGene id newGen
  where (id, newGen) = randomR (0, pred numGenes) gen
