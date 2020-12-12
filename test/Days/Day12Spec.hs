module Days.Day12Spec where

import Test.Hspec

import Days.Day12

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartA "input/Day12.txt"
      result `shouldBe` 2228
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day12.txt"
      result `shouldBe` 42908