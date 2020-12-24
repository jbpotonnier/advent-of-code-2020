module Main (main) where

import Aoc
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Aoc" $ do
    it "parse" $ do
      parseLine "eseneeswwnw"
        `shouldBe` [ (1, -1, 0),
                     (0, -1, 1),
                     (1, 0, -1),
                     (1, -1, 0),
                     (-1, 0, 1),
                     (-1, 1, 0),
                     (0, 1, -1)
                   ]
    it "toCoord" $ do
      toCoord [(1, -1, 0), (0, -1, 1), (-1, 1, 0)] `shouldBe` (0, -1, 1)

    it "star 1" $ do
      (length . blackTiles <$> readInput "./test/example.txt")
        `shouldReturn` 10

      (length . blackTiles <$> readInput "./test/input.txt")
        `shouldReturn` 322