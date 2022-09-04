module TicTacToe.CoordinateSpec where

import AppPrelude

import Test.Hspec
import Test.QuickCheck

import qualified TicTacToe.Coordinate as Coordinate

spec :: Spec
spec = describe "TicTacToe.Coordinate" $ do
  describe "new" $ do
    let (Just c1) = Coordinate.new 1 2
    let (Just c2) = Coordinate.new 2 3

    it "sets the row" $ do
      Coordinate.row c1 `shouldBe` 1
      Coordinate.row c2 `shouldBe` 2

    it "sets the column" $ do
      Coordinate.column c1 `shouldBe` 2
      Coordinate.column c2 `shouldBe` 3

    it "returns Nothing for a row less than 1" $
      Coordinate.new 0 1 `shouldBe` Nothing

    it "returns Nothing for a row greater than 3" $
      Coordinate.new 4 1 `shouldBe` Nothing

    it "returns Nothing for a column less than 1" $
      Coordinate.new 1 0 `shouldBe` Nothing

    it "returns Nothing for a column greater than 3" $
      Coordinate.new 1 4 `shouldBe` Nothing

  describe "fromIndex" $ do
    it "returns (1, 1) for index of 1" $ do
      let (Just c) = Coordinate.fromIndex 1
      Coordinate.row c `shouldBe` 1
      Coordinate.column c `shouldBe` 1

    it "returns (1, 2) for index of 2" $ do
      let (Just c) = Coordinate.fromIndex 2
      Coordinate.row c `shouldBe` 1
      Coordinate.column c `shouldBe` 2

    it "returns (1, 3) for index of 3" $ do
      let (Just c) = Coordinate.fromIndex 3
      Coordinate.row c `shouldBe` 1
      Coordinate.column c `shouldBe` 3

    it "returns (2, 1) for index of 4" $ do
      let (Just c) = Coordinate.fromIndex 4
      Coordinate.row c `shouldBe` 2
      Coordinate.column c `shouldBe` 1

    it "returns Nothing for index less than 1" $ property $
      \(Positive x) -> Coordinate.fromIndex (negate x) `shouldBe` Nothing

    it "returns Nothing for index greater than 10" $ property $
      \(Positive x) -> Coordinate.fromIndex (10 + x) `shouldBe` Nothing

  describe "allCoordinates" $
    it "returns a list of all Coordinates" $ do
      let results = Coordinate.toTuple <$> Coordinate.allCoordinates
      results `shouldBe` [ (1, 1), (1, 2), (1, 3)
                         , (2, 1), (2, 2), (2, 3)
                         , (3, 1), (3, 2), (3, 3)
                         ]

  describe "allRows" $
    it "returns a list of all row Coordinates" $ do
      let results = fmap Coordinate.toTuple <$> Coordinate.allRows
      results `shouldBe` [ [ (1, 1), (1, 2), (1, 3) ]
                         , [ (2, 1), (2, 2), (2, 3) ]
                         , [ (3, 1), (3, 2), (3, 3) ]
                         ]

  describe "allColumns" $
    it "returns a list of all column Coordinates" $ do
      let results = fmap Coordinate.toTuple <$> Coordinate.allColumns
      results `shouldBe` [ [ (1, 1), (2, 1), (3, 1) ]
                         , [ (1, 2), (2, 2), (3, 2) ]
                         , [ (1, 3), (2, 3), (3, 3) ]
                         ]

  describe "allDiagonals" $
    it "returns a list of all diagonal Coordinates" $ do
      let results = fmap Coordinate.toTuple <$> Coordinate.allDiagonals
      results `shouldBe` [ [ (1, 1), (2, 2), (3, 3) ]
                         , [ (1, 3), (2, 2), (3, 1) ]
                         ]
