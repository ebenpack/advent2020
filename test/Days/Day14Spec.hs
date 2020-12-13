module Days.Day14Spec where

import Test.Hspec

import Days.Day14

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day14.txt"
       result `shouldBe` 1016964
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day14.txt"
      result `shouldBe` 182588480

