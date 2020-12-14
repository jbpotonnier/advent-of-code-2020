{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Aoc
import Test.Hspec

data Bus = Bus {delta :: Integer, freq :: Integer}
  deriving (Show, Eq)

star2 :: [Maybe Integer] -> Integer
star2 = solve 0 1 . convert

solve :: Integer -> Integer -> [Bus] -> Integer
solve timestamp jump = \case
  [] -> timestamp
  b : bs ->
    let (ts, jmp) = g b timestamp jump
     in solve ts jmp bs

g :: Bus -> Integer -> Integer -> (Integer, Integer)
g b@Bus {delta, freq} ts jmp
  | (ts + delta) `rem` freq == 0 = (ts, jmp * freq)
  | otherwise = g b (ts + jmp) jmp

convert :: [Maybe Integer] -> [Bus]
convert =
  mapMaybe (\case (_, Nothing) -> Nothing; (i, Just bid) -> Just (Bus i bid))
    . zip [0 ..]

main :: IO ()
main = hspec $ do
  describe "Aoc" $ do
    it "star 1" $ do
      star1 939 [7, 13, 59, 31, 19] `shouldBe` 295
      star1 1002462 [37, 41, 601, 19, 17, 23, 29, 443, 13] `shouldBe` 3606

    it "convert" $ do
      convert [Just 7, Just 13, Nothing, Nothing, Just 59, Nothing, Just 31, Just 19]
        `shouldBe` [Bus {freq = 7, delta = 0}, Bus {freq = 13, delta = 1}, Bus {freq = 59, delta = 4}, Bus {freq = 31, delta = 6}, Bus {freq = 19, delta = 7}]

    it "star 2" $ do
      star2 [Just 7, Just 13, Nothing, Nothing, Just 59, Nothing, Just 31, Just 19]
        `shouldBe` 1068781

      star2 [Just 17, Nothing, Just 13, Just 19] `shouldBe` 3417

      star2 [Just 67, Just 7, Just 59, Just 61] `shouldBe` 754018

      star2 [Just 67, Nothing, Just 7, Just 59, Just 61] `shouldBe` 779210

      star2 [Just 67, Just 7, Nothing, Just 59, Just 61] `shouldBe` 1261476

      star2 [Just 1789, Just 37, Just 47, Just 1889] `shouldBe` 1202161486

    it "star 2 result" $ do
      star2
        [Just 37, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 41, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 601, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 19, Nothing, Nothing, Nothing, Nothing, Just 17, Nothing, Nothing, Nothing, Nothing, Nothing, Just 23, Nothing, Nothing, Nothing, Nothing, Nothing, Just 29, Nothing, Just 443, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 13]
        `shouldBe` 379786358533423