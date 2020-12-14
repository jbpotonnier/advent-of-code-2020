module Aoc
  ( module Aoc,
  )
where

import Data.List (minimumBy)
import Data.Maybe (fromJust)

listBuses :: [Integer] -> [(Integer, [Integer])]
listBuses xs = [(x, [0, x ..]) | x <- xs]

star1 :: Integer -> [Integer] -> Integer
star1 t xs = (time - t) * busId
  where
    (busId, time) = minimumBy (compare `on` snd) . fmap g . listBuses $ xs

    g (b, ys) = (b, (head' . filter (>= t)) ys)

-----------------------------------

-- applyTimes :: (c -> c) -> Int -> c -> c
-- applyTimes f n = (!! n) . iterate f

-- count :: (Ord k, Foldable t) => t k -> k -> Int
-- count xs x = fromMaybe 0 (c IMap.!? x)
--   where
--     c = IMap.fromListWith (+) . fmap (,1) . toList $ xs

-- readInt :: Text -> Maybe Int
-- readInt = readMaybe . toString

head' :: [c] -> c
head' = fromJust . viaNonEmpty head

-- headMay :: [b] -> Maybe b
-- headMay = viaNonEmpty head

-- tail' :: [a] -> [a]
