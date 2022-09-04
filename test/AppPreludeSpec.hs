{-# LANGUAGE DeriveAnyClass #-}

module AppPreludeSpec where

import AppPrelude

import Test.Hspec

data TestEnum = One | Two
  deriving (CycleEnum, Bounded, Enum, Eq, Show)

spec :: Spec
spec = describe "Prelude" $
  describe "CycleEnum" $ do
    describe "cpred" $ do
      it "should cycle on the min bound" $
        cpred One `shouldBe` Two

      it "should return the predecessor" $
        cpred Two `shouldBe` One

    describe "csucc" $ do
      it "should cycle on the max bound" $
        csucc Two `shouldBe` One

      it "should return the predecessor" $
        csucc One `shouldBe` Two
