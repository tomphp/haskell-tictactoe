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

convertStatement :: Gene -> ParseTree
convertStatement (Rank gene) = Result gene

ifAnd :: Gene -> Gene -> Gene -> ParseTree
ifAnd cond true false = IfAndNode (convertCondition cond) (convertStatement true) (convertStatement false)

parseIfAnd :: DNA -> Maybe (ParseTree, DNA)
parseIfAnd dna = do
  (cond, rem1)  <- findGeneType dna isCondition
  (true, rem2)  <- findGeneType rem1 isStatement
  (false, rem3) <- findGeneType rem2 isStatement
  Just (ifAnd cond true false, rem3)

parseStatement :: (Gene, DNA) -> Maybe (ParseTree, DNA)
parseStatement (gene, dna) = case gene of
  IfAnd      -> parseIfAnd dna
  Rank value -> Just (Result value, dna)
  _          -> Nothing

parse :: DNA -> Maybe ParseTree
parse dna = do
  dna'   <- findGeneType dna isStatement
  result <- parseStatement dna'
  Just $ fst result

