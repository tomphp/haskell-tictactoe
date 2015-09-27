module TicTacToe.AI.GeneratorSpec where

import System.Random

import Test.Hspec

import TicTacToe.AI.DNA
import TicTacToe.AI.Generator

-- This spec uses magic random seeds (fragile!)
spec :: Spec
spec = do
  describe "TicTacToe.AI.Generator" $ do
    context "randomGene" $ do
      it "generates an IfAnd" $ do
        let gen = mkStdGen 99676

        fst (randomGene gen) `shouldBe` IfAnd

      it "generates an IfOr" $ do
        let gen = mkStdGen 5

        fst (randomGene gen) `shouldBe` IfOr

      it "generates a CurrentCellIs" $ do
        let gen = mkStdGen 99195

        fst (randomGene gen) `shouldBe` CurrentCellIs 3


