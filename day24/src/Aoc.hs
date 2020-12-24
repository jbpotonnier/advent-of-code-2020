module Aoc
  ( module Aoc,
  )
where

import qualified Data.Map.Strict as Map

blackTiles :: [(Int, Int, Int)] -> [(Int, Int, Int)]
blackTiles = fmap fst . filter (odd . snd) . Map.toList . count

add :: (Num a, Num b, Num c) => (a, b, c) -> (a, b, c) -> (a, b, c)
add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

toCoord :: [(Int, Int, Int)] -> (Int, Int, Int)
toCoord = foldl' add (0, 0, 0)

parseLine :: Text -> [(Int, Int, Int)]
parseLine = go . toString
  where
    go = \case
      [] -> []
      's' : 'w' : rs -> (-1, 0, 1) : go rs
      's' : 'e' : rs -> (0, -1, 1) : go rs
      'n' : 'e' : rs -> (1, 0, -1) : go rs
      'n' : 'w' : rs -> (0, 1, -1) : go rs
      'e' : rs -> (1, -1, 0) : go rs
      'w' : rs -> (-1, 1, 0) : go rs
      d -> error $ "unknown direction" <> show d

readInput :: MonadIO f => FilePath -> f [(Int, Int, Int)]
readInput path = fmap (toCoord . parseLine) . lines <$> readFileText path

count :: (Ord k) => [k] -> Map k Int
count = Map.fromListWith (+) . fmap (,1)
