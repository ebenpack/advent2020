module Days.Day20Spec where

import Test.Hspec

import Days.Day20

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day20.txt"
       result `shouldBe` 17712468069479
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day20.txt"
      result `shouldBe` 182588480

