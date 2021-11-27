module Days.Day19Spec where

import Test.Hspec

import Days.Day19

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day19.txt"
       result `shouldBe` 203
  -- describe "partB" $ do
  --   it "calculates the correct answer" $ do
  --     result <- runDayPartB "input/Day19.txt"
  --     result `shouldBe` 145575710203332

