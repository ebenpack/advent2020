module Days.Day18Spec where

import Test.Hspec

import Days.Day18

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day18.txt"
       result `shouldBe` 8298263963837
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day18.txt"
      result `shouldBe` 145575710203332

