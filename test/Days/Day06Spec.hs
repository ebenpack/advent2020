module Days.Day06Spec where

import Test.Hspec

import Days.Day06

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day06.txt"
       result `shouldBe` 6878
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day06.txt"
      result `shouldBe` 3464

