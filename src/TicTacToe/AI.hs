module TicTacToe.AI (parse) where

import TicTacToe.AI.DNA
import TicTacToe.AI.ParseTree

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

extract :: Maybe (Gene, DNA) -> (Gene, DNA)
extract (Just result) = result

parseIfAnd :: DNA -> (ParseTree, DNA)
parseIfAnd dna = (ifAnd cond true false, rem3)
  -- hack until I work out how to do it properly
  where (cond, rem1)  = extract $ findGeneType dna isCondition
        (true, rem2)  = extract $ findGeneType rem1 isResult
        (false, rem3) = extract $ findGeneType rem2 isResult

parse :: DNA -> Maybe ParseTree
parse dna = case dna' of
  Just (IfAnd, sequence) -> Just $ fst $ parseIfAnd sequence
  Just (Rank value, _)   -> Just $ Result value
  _                      -> Nothing
  where dna' = findGeneType dna isResult

