module Days.Day07Spec where

import Test.Hspec

import Days.Day07

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day07.txt"
       result `shouldBe` 148
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day07.txt"
      result `shouldBe` 24867

