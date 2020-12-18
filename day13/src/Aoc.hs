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

head' :: [c] -> c
head' = fromJust . viaNonEmpty head
