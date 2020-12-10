module Days.Day02Spec where

import Test.Hspec

import Days.Day02

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day02.txt"
       result `shouldBe` 454
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day02.txt"
      result `shouldBe` 649
