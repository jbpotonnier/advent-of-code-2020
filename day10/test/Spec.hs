module Main (main) where

import Day10
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day5" $ do
    it "differences" $ do
      differences [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
        `shouldBe` [1, 3, 1, 1, 1, 3, 1, 1, 3, 1, 3, 3]

      differences [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]
        `shouldBe` [1, 1, 1, 1, 3, 1, 1, 1, 1, 3, 3, 1, 1, 1, 3, 1, 1, 3, 3, 1, 1, 1, 1, 3, 1, 3, 3, 1, 1, 1, 1, 3]

      (count . differences) [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]
        `shouldBe` fromList [(1, 22), (3, 10)]

    it "count" $ do
      count (["a", "a", "b"] :: [Text]) `shouldBe` fromList [("a", 2), ("b", 1)]

    it "first star" $ do
      Just xs <- readInput readInt "./test/input.txt"
      length xs `shouldBe` 94
      (count . differences) xs `shouldBe` fromList [(1, 68), (3, 27)]

    it "should count arrangements" $ do
       star2 [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]
        `shouldBe` 19208

    it "second star" $ do
      Just xs <- readInput readInt "./test/input.txt"
      star2 xs `shouldBe` 43406276662336