module TicTacToe.AI.GeneratorSpec where

import System.Random

import Test.Hspec

import TicTacToe.AI.DNA
import TicTacToe.AI.Generator

nextRand :: (Gene, StdGen) -> Int
nextRand (_, gen) = fst $ randomR (0, 100) gen

-- This spec does use some magic random values
spec :: Spec
spec = do
  describe "TicTacToe.AI.Generator" $ do
    context "randomGene" $ do
      context "IfAnd" $ do
        let seq = [ (0, mkStdGen 0) ]

        it "generates the Gene" $ do
          fst (randomGene seq) `shouldBe` IfAnd

        it "returns the next generator" $ do
          nextRand (randomGene seq) `shouldBe` 25

      context "IfOr" $ do
        let seq = [ (1, mkStdGen 0) ]

        it "generates the Gene" $ do
          fst (randomGene seq) `shouldBe` IfOr

        it "returns the next generator" $ do
          nextRand (randomGene seq) `shouldBe` 25

      context "CurrentCellIs" $ do
        let seq = [ (2, mkStdGen 0)
                  , (5, mkStdGen 1) ]

        it "generates the Gene" $ do
          fst (randomGene seq) `shouldBe` CurrentCellIs 5

        it "returns the next generator" $ do
          nextRand (randomGene seq) `shouldBe` 95
