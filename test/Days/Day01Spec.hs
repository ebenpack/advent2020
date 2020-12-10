module Days.Day01Spec where

import Test.Hspec

import Days.Day01

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day01.txt"
       result `shouldBe` Just 1016964
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day01.txt"
      result `shouldBe` Just 182588480

