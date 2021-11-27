module Days.Day17Spec where

import Test.Hspec

import Days.Day17

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day17.txt"
       result `shouldBe` 207
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day17.txt"
      result `shouldBe` 182588480

