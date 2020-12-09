module Main (main) where

import Data.List (maximum, minimum)
import Day9
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day9" $ do
    it "should readInput" $ do
      readInput "./test/example.txt"
        `shouldReturn` Just [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]

    it "should validate" $ do
      let preamble = [1 .. 25]
      36 `shouldSatisfy` validate preamble
      49 `shouldSatisfy` validate preamble
      100 `shouldNotSatisfy` validate preamble
      50 `shouldNotSatisfy` validate preamble

    it "should find invalid" $ do
      Just input <- readInput "./test/example.txt"
      findFirstInvalid 5 input `shouldBe` 127

    it "should find first star" $ do
      Just input <- readInput "./test/input.txt"
      length input `shouldBe` 1000
      findFirstInvalid 25 input `shouldBe` 2089807806

    it "should find contiguous set" $ do
      Just input <- readInput "./test/example.txt"
      findContiguousSet input 127 `shouldBe` [[15, 25, 47, 40]]

    it "should find second star" $ do
      Just input <- readInput "./test/input.txt"
      let expected = [106599441, 133274893, 91957895, 97197365, 104060212, 142165569, 113972262, 109668776, 109871715, 115904771, 149614315, 122120055, 153890744, 122322994, 131935128, 140622865, 144628806]
      findContiguousSet input 2089807806 `shouldBe` [expected]
      sum [minimum expected, maximum expected] `shouldBe` 245848639
