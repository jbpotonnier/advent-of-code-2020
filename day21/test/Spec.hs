module Main (main) where

import Aoc
import qualified Data.Text as T
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

    it "eliminate" $ do
      (_, m) <- readInput "./test/example.txt"
      eliminate m
        `shouldBe` fromList
          [ ("dairy", fromList ["mxmxvkd"]),
            ("fish", fromList ["sqjhc"]),
            ("soy", fromList ["fvjkl"])
          ]

    it "star 2" $ do
      (_, m) <- readInput "./test/input.txt"
      let sol = converge eliminate m
      sol `shouldSatisfy` isSolution
      (T.intercalate "," . fmap snd . sort . toSolution $ sol)
        `shouldBe` "rcqb,cltx,nrl,qjvvcvz,tsqpn,xhnk,tfqsb,zqzmzl"
