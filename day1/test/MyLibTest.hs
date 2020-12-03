module Main (main) where

import MyLib (sumsTo2020, sumsTo2020')
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day1" $ do
    describe "First puzzle" $ do
      it "example" $ do
        input <- readFile "./test/example.txt"
        sumsTo2020 input `shouldBe` 514579

    describe "Second puzzle" $ do
      it "example" $ do
        input <- readFile "./test/example.txt"
        sumsTo2020' input `shouldBe` 241861950
