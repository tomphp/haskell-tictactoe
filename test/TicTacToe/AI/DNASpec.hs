module TicTacToe.AI.DNASpec where

import Test.Hspec
import TicTacToe.AI.DNA

spec :: Spec
spec = do
  describe "TicTacToe.AI.DNA" $ do
    context "findGeneType" $ do
      it "finds a gene by type" $ do
        let dna = [CellIsEmpty 1, Rank 2, CellIsNotEmpty 3]

        findGeneType dna isStatement `shouldBe` Just (Rank 2, [CellIsNotEmpty 3])

      it "returns nothing if match is not found" $ do
        let dna = [CellIsEmpty 1, CellIsNotEmpty 3]

        findGeneType dna isStatement `shouldBe` Nothing
