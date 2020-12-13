module Days.Day13Spec where

import Test.Hspec

import Days.Day13

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day13.txt"
       result `shouldBe` 138
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day13.txt"
      result `shouldBe` 226845233210288

