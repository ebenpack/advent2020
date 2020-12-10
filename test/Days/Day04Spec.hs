module Days.Day04Spec where

import Test.Hspec

import Days.Day04

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day04.txt"
       result `shouldBe` 206
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day04.txt"
      result `shouldBe` 123

