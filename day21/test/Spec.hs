module Main (main) where

import Aoc
import qualified Data.Map as Map
import Data.Set (difference, member)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Aoc" $ do
    it "parse" $ do
      readLine "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
        `shouldBe` (["mxmxvkd", "kfcds", "sqjhc", "nhms"], ["dairy", "fish"])

    it "read example" $ do
      (ingredients, m) <- readInput "./test/example.txt"
      m
        `shouldBe` fromList
          [ ("dairy", fromList ["mxmxvkd"]),
            ("fish", fromList ["mxmxvkd", "sqjhc"]),
            ("soy", fromList ["fvjkl", "sqjhc"])
          ]

      ingredients
        `shouldBe` [ "mxmxvkd",
                     "kfcds",
                     "sqjhc",
                     "nhms",
                     "trh",
                     "fvjkl",
                     "sbzzf",
                     "mxmxvkd",
                     "sqjhc",
                     "fvjkl",
                     "sqjhc",
                     "mxmxvkd",
                     "sbzzf"
                   ]

    it "first star" $ do
      inp <- readInput "./test/input.txt"
      length (inerts inp) `shouldBe` 2659

