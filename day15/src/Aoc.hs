module Aoc
  ( module Aoc,
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Relude.Extra ((!?))

speak :: [Int] -> Int -> Int
speak xs end = next (length start) (end - 1) initSeen (last' xs)
  where
    initSeen = Map.fromList [(i, v) | (i, v) <- zip start [0 ..]]
    start = init' xs

next :: Int -> Int -> Map Int Int -> Int -> Int
next i end seen x
  | i == end = x
  | otherwise =
    let x' = seen !? x & maybe 0 (i -)
     in next (i + 1) end (Map.insert x i seen) x'

-----------------------------------
last' :: [c] -> c
last' = fromJust . viaNonEmpty last

init' :: [a] -> [a]
init' = fromJust . viaNonEmpty init