module Main (main) where

import Aoc
import Data.List ((!!))
import Test.Hspec

main :: IO ()
main = hspec $
  describe "Aoc" $ do
    it "example 1" $ do
      (take 10 . speak) [0, 3, 6] `shouldBe` [0, 3, 6, 0, 3, 3, 1, 0, 4, 0]
      (speak [0, 3, 6] !! 2019) `shouldBe` 436

      (speak [1, 3, 2] !! 2019) `shouldBe` 1
      (speak [2, 1, 3] !! 2019) `shouldBe` 10
      (speak [1, 2, 3] !! 2019) `shouldBe` 27
      (speak [2, 3, 1] !! 2019) `shouldBe` 78
      (speak [3, 2, 1] !! 2019) `shouldBe` 438
      (speak [3, 1, 2] !! 2019) `shouldBe` 1836

      -- star 1
      (speak [2, 0, 1, 7, 4, 14, 18] !! 2019) `shouldBe` 496

    it "example 2" $ do
      -- speak [0, 3, 6] !! (30000000 -1) `shouldBe` 175594

      -- star 2
      speak [2, 0, 1, 7, 4, 14, 18] !! (30000000 -1) `shouldBe` 175594
