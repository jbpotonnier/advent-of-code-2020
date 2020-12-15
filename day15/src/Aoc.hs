module Aoc
  ( module Aoc,
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

speak :: [Int] -> [Int]
speak xs = start ++ next (length start) initSeen (last' xs)
  where
    initSeen = Map.fromList [(i, v) | (i, v) <- zip start [0 ..]]
    start = init' xs

next :: Int -> Map Int Int -> Int -> [Int]
next i seen x =
  let x' = case seen Map.!? x of
        Nothing -> 0
        Just b -> i - b
   in x : next (i + 1) (Map.insert x i seen) x'

-----------------------------------

-- readInput path = mapMaybe (readLine . toString) . lines <$> readFileText path
--   where
--     readLine l = Just ()

-- applyTimes :: (c -> c) -> Int -> c -> c
-- applyTimes f n = (!! n) . iterate f

-- count :: (Ord k, Foldable t) => t k -> k -> Int
-- count xs x = fromMaybe 0 (c IMap.!? x)
--   where
--     c = IMap.fromListWith (+) . fmap (,1) . toList $ xs

-- readInt :: Text -> Maybe Int
-- readInt = readMaybe . toString

-- head' :: [c] -> c

last' :: [c] -> c
last' = fromJust . viaNonEmpty last

init' :: [a] -> [a]
init' = fromJust . viaNonEmpty init

-- headMay :: [b] -> Maybe b
-- headMay = viaNonEmpty head

-- tail' :: [a] -> [a]
