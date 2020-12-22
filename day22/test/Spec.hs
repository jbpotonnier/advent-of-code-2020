module Main (main) where

import Aoc
import Test.Hspec (xit, describe, hspec, it, shouldBe)

example :: Game
example =
  Game
    { player1 =
        fromList [9, 2, 6, 3, 1],
      player2 =
        fromList [5, 8, 4, 7, 10]
    }

input :: Game
input =
  Game
    { player1 =
        fromList
          [41, 48, 12, 6, 1, 25, 47, 43, 4, 35, 10, 13, 23, 39, 22, 28, 44, 42, 32, 31, 24, 50, 34, 29, 14],
      player2 =
        fromList
          [36, 49, 11, 16, 20, 17, 26, 30, 18, 5, 2, 38, 7, 27, 21, 9, 19, 15, 8, 45, 37, 40, 33, 46, 3]
    }

main :: IO ()
main = hspec $ do
  describe "Aoc" $ do
    it "star 1" $ do
      score (play example) `shouldBe` 306
      score (play input) `shouldBe` 32856

    it "star 2 example" $ do
      (score . snd . play2) example `shouldBe` 291

    x it "star 2 " $ do
      (score . snd . play2) input `shouldBe` 0
