module Main (main) where

import Aoc
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Aoc" $ do
    it "Manhattan distance" $ do
      pending