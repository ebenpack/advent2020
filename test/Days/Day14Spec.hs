module Days.Day14Spec where

import Test.Hspec

import Days.Day14

spec :: Spec
spec = do
  describe "partA" $ do
    it "calculates the correct answer" $ do
       result <- runDayPartA "input/Day14.txt"
       result `shouldBe` 17481577045893
  describe "partB" $ do
    it "calculates the correct answer" $ do
      result <- runDayPartB "input/Day14.txt"
      result `shouldBe` 4160009892257

