module Days.Day05Spec where

import Test.Hspec

import Days.Day05

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day05.txt"
       result `shouldBe` 953
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day05.txt"
      result `shouldBe` 615

