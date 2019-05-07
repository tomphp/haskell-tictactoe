module TicTacToe.PlayerSpec where

import Test.Hspec

import TicTacToe.Player (Player(..), switch)

spec :: Spec
spec = describe "TicTacToe.Player" $ do
  describe "show" $
    it "shows the player" $ do
      show Naughts `shouldBe` "Naughts"
      show Crosses `shouldBe` "Crosses"

  describe "switch" $
    it "switch players" $ do
      switch Naughts `shouldBe` Crosses
      switch Crosses `shouldBe` Naughts
