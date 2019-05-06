module TicTacToe.PlayerSpec where

import Test.Hspec

import TicTacToe.Player (Player(..))

spec :: Spec
spec = describe "TicTacToe.Player" $
  describe "show" $
    it "shows the player" $ do
      show Naughts `shouldBe` "Naughts"
      show Crosses `shouldBe` "Crosses"
