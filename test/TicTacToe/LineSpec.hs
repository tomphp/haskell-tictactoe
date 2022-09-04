module TicTacToe.LineSpec where

import AppPrelude

import Test.Hspec

import TicTacToe.Line (Line(..))
import TicTacToe.Line as Line

data Player = P1 | P2 deriving (Eq, Show)

spec :: Spec
spec = describe "TicTacToe.Line" $ do
  describe "winner" $ do
    it "returns Nothing when the line is empty" $ do
      Line.winner (Line [Nothing, Nothing, Nothing]) `shouldBe` Nothing @(Maybe Player)

    it "returns Just P1 when P1 wins" $ do
      Line.winner (Line [Just P1, Just P1, Just P1]) `shouldBe` Just P1

    it "returns Just P2 when P2 wins" $ do
      Line.winner (Line [Just P2, Just P2, Just P2]) `shouldBe` Just P2

    it "returns Nothing with mixed line" $ do
      Line.winner (Line [Just P2, Just P1, Just P2]) `shouldBe` Nothing
