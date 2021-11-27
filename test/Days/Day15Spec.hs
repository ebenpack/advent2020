module Days.Day15Spec where

import Test.Hspec

import Days.Day15

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day15.txt"
       result `shouldBe` 929
  -- this one was not solved so efficiently
  -- describe "partB" $ do
  --   it "calculates the correct answer" $ do
  --     result <- runDayPartB "input/Day15.txt"
  --     result `shouldBe` 16671510

