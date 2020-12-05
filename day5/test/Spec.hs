{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable (Foldable (maximum))
import Day5
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day 5" $ do
    it "should parse boarding pass" $ do
      parseBoardingPass "BFFFBBFRRL"
        `shouldBe` Just
          ( BoardingPass
              [High, Low, Low, Low, High, High, Low]
              [High, High, Low]
          )

    it "should compute row" $ do
      computeRow [Low, High, Low, High, High, Low, Low] `shouldBe` 44

    it "should compute column" $ do
      computeColumn [High, Low, High] `shouldBe` 5

    it "should compute seat id" $ do
      seatId "BFFFBBFRRR" `shouldBe` Just 567
      seatId "FFFBBBFRRR" `shouldBe` Just 119
      seatId "BBFFBBFRLL" `shouldBe` Just 820

    it "should compute first star" $ do
      content <- readFileText "./test/input.txt"
      let seatIds = traverse seatId . lines $ content
      maximum <$> seatIds `shouldBe` Just 864

    it "should compute second star" $ do
      content <- readFileText "./test/input.txt"
      let boardingPasses = lines content
      mySeat boardingPasses `shouldBe` Just (fromList [739])