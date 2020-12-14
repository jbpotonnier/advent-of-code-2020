{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Aoc
import Test.Hspec

data Bus = Bus {busId :: Integer, busPos :: Integer}
  deriving (Show, Eq)

star2 :: [Maybe Integer] -> Integer
star2 xs = head' (filter (satisfyAll filters) (enumerate firstBus))
  where
    filters = fmap mkPred bs

    firstBus = head' bs

    enumerate (Bus {busId, busPos}) = [n - busPos | n <- [0, busId ..]]

    bs :: [Bus]
    bs = sortOn (\b -> - (busId b)) . convert $ xs

convert :: [Maybe Integer] -> [Bus]
convert = catMaybes . zipWith g [0 ..]
  where
    g i c = case c of
      Just n -> Just $ Bus n i
      Nothing -> Nothing

isMultiple :: Integral a => a -> a -> Bool
isMultiple n m = n `rem` m == 0

satisfyAll :: Foldable t1 => t1 (t2 -> Bool) -> t2 -> Bool
satisfyAll = foldl' pAnd (const True)
  where
    pAnd p1 p2 x = p1 x && p2 x

mkPred :: Bus -> Integer -> Bool
mkPred Bus {busId, busPos} n = (n + busPos) `isMultiple` busId

main :: IO ()
main = hspec $ do
  describe "Aoc" $ do
    it "star 1" $ do
      star1 939 [7, 13, 59, 31, 19] `shouldBe` 295
      star1 1002462 [37, 41, 601, 19, 17, 23, 29, 443, 13] `shouldBe` 3606

    it "convert" $ do
      convert [Just 7, Just 13, Nothing, Nothing, Just 59, Nothing, Just 31, Just 19]
        `shouldBe` [Bus {busId = 7, busPos = 0}, Bus {busId = 13, busPos = 1}, Bus {busId = 59, busPos = 4}, Bus {busId = 31, busPos = 6}, Bus {busId = 19, busPos = 7}]

    it "star 2" $ do
      star2 [Just 7, Just 13, Nothing, Nothing, Just 59, Nothing, Just 31, Just 19]
        `shouldBe` 1068781

      star2 [Just 17, Nothing, Just 13, Just 19] `shouldBe` 3417

      star2 [Just 67, Just 7, Just 59, Just 61] `shouldBe` 754018

      star2 [Just 67, Nothing, Just 7, Just 59, Just 61] `shouldBe` 779210

      star2 [Just 67, Just 7, Nothing, Just 59, Just 61] `shouldBe` 1261476

      star2 [Just 1789, Just 37, Just 47, Just 1889] `shouldBe` 1202161486

    fit "star 2 result" $ do
      star2
        [Just 37, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 41, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 601, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 19, Nothing, Nothing, Nothing, Nothing, Just 17, Nothing, Nothing, Nothing, Nothing, Nothing, Just 23, Nothing, Nothing, Nothing, Nothing, Nothing, Just 29, Nothing, Just 443, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 13]
        `shouldBe` 1