module Days.Day16Spec where

import Test.Hspec

import Days.Day16

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day16.txt"
       result `shouldBe` 23954
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day16.txt"
      result `shouldBe` 453459307723

