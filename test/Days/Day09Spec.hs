module Days.Day09Spec where

import Test.Hspec

import Days.Day09

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day09.txt"
       result `shouldBe` 104054607
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day09.txt"
      result `shouldBe` 13935797

