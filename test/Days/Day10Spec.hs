module Days.Day10Spec where

import Test.Hspec

import Days.Day10

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day10.txt"
       result `shouldBe` 2812
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day10.txt"
      result `shouldBe` 386869246296064

