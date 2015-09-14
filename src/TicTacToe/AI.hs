module TicTacToe.AI where

import System.Random
import TicTacToe.Board

type RankValue = Int

data Gene = IfAnd | IfOr |
            CurrentCellIs CellIndex |
            CellIsEmpty CellIndex | CellIsNotEmpty CellIndex |
            CellIsMine CellIndex | CellIsTheirs CellIndex |
            Rank RankValue deriving (Eq, Show)

type DNA = [Gene]

data Condition = CurrentCellIsCond CellIndex |
                 CellIsEmptyCond CellIndex |
                 CellIsNotEmptyCond CellIndex |
                 CellIsMineCond CellIndex |
                 CellIsTheirsCond CellIndex deriving (Show, Eq)

data ParseTree = IfAndNode Condition ParseTree ParseTree | Result RankValue deriving (Show, Eq)

isCondition :: Gene -> Bool
isCondition gene = case gene of
  CurrentCellIs _  -> True
  CellIsEmpty _    -> True
  CellIsNotEmpty _ -> True
  CellIsMine _     -> True
  CellIsTheirs _   -> True
  _                -> False

isResult :: Gene -> Bool
isResult gene = case gene of
  IfAnd  -> True
  IfOr   -> True
  Rank _ -> True
  _      -> False

findGeneType :: DNA -> (Gene -> Bool) -> (Gene, DNA)
findGeneType dna matchFunc = (head dna', tail dna')
  where dna' = dropWhile (not . matchFunc) dna

convertCondition :: Gene -> Condition
convertCondition gene = case gene of
  CurrentCellIs index  -> CurrentCellIsCond index
  CellIsEmpty index    -> CellIsEmptyCond index
  CellIsNotEmpty index -> CellIsNotEmptyCond index
  CellIsMine index     -> CellIsMineCond index
  CellIsTheirs index   -> CellIsTheirsCond index

convertResult :: Gene -> ParseTree
convertResult (Rank gene) = Result gene

ifAnd :: Gene -> Gene -> Gene -> ParseTree
ifAnd cond true false = IfAndNode (convertCondition cond) (convertResult true) (convertResult false)

parseIfAnd :: DNA -> (ParseTree, DNA)
parseIfAnd dna = (ifAnd cond true false, rem3)
  where (cond, rem1) = findGeneType dna isCondition
        (true, rem2) = findGeneType rem1 isResult
        (false, rem3)   = findGeneType rem2 isResult

parse :: DNA -> Maybe ParseTree
parse dna = case dna' of
  IfAnd:sequence -> Just $ fst $ parseIfAnd sequence
  Rank value:_   -> Just $ Result value
  _              -> Nothing
  where dna' = dropWhile (not . isResult) dna

