module Main (main) where

import Aoc
import Test.Hspec

main :: IO ()
main = hspec $
  describe "Aoc" $ do
    it "example 1" $ do
      speak [0, 3, 6] 2020 `shouldBe` 436

      speak [1, 3, 2] 2020 `shouldBe` 1
      speak [2, 1, 3] 2020 `shouldBe` 10
      speak [1, 2, 3] 2020 `shouldBe` 27
      speak [2, 3, 1] 2020 `shouldBe` 78
      speak [3, 2, 1] 2020 `shouldBe` 438
      speak [3, 1, 2] 2020 `shouldBe` 1836

      -- star 1
      speak [2, 0, 1, 7, 4, 14, 18] 2020 `shouldBe` 496

    it "example 2" $
      speak [2, 0, 1, 7, 4, 14, 18] 30000000 `shouldBe` 883
