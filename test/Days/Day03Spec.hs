module Days.Day03Spec where

import Test.Hspec

import Days.Day03

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day03.txt"
       result `shouldBe` 223
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day03.txt"
      result `shouldBe` 3517401300
