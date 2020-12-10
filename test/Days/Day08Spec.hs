module Days.Day08Spec where

import Test.Hspec

import Days.Day08

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day08.txt"
       result `shouldBe` 1446
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day08.txt"
      result `shouldBe` 1403

