module Main (main) where

import Aoc
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Aoc" $ do
    it "parse example" $ do
      tiles <- readInput "./test/example.txt"
      traverse_ print tiles
      length tiles `shouldBe` 9

    it "parse input" $ do
      tiles <- readInput "./test/input.txt"
      traverse_ print tiles
      length tiles `shouldBe` 144