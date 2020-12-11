module Days.Day11Spec where

import Test.Hspec

import Days.Day11

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day11.txt"
       result `shouldBe` 2386
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day11.txt"
      result `shouldBe` 2091

