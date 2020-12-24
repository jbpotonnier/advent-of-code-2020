module Aoc
  ( module Aoc,
  )
where

import Data.Foldable (Foldable (maximum, minimum))
import qualified Data.Map.Strict as Map
import Data.Set (intersection)
import qualified Data.Set as Set

applyRules :: Set (Int, Int, Int) -> Set (Int, Int, Int)
applyRules ts = Set.filter nextIsBlack . allTiles $ ts
  where
    nextIsBlack t
      | isBlack t =
        let blackCount = countBlackNeibors t
         in not (blackCount == 0 || blackCount > 2)
      | otherwise = countBlackNeibors t == 2

    countBlackNeibors t = Set.size $ neighbors t `intersection` ts

    isBlack t = Set.member t ts

neighbors :: (Int, Int, Int) -> Set (Int, Int, Int)
neighbors c =
  fromList . fmap (add c) $
    [(-1, 0, 1), (0, -1, 1), (1, 0, -1), (0, 1, -1), (1, -1, 0), (-1, 1, 0)]

allTiles :: Set (Int, Int, Int) -> Set (Int, Int, Int)
allTiles ls =
  fromList
    [ (x, y, z)
      | x <- [minX .. maxX],
        y <- [minY .. maxY],
        z <- [minZ .. maxZ],
        x + y + z == 0
    ]
  where
    minX = (\x -> x - 1) . minimum . Set.map getX $ ls
    maxX = (+ 1) . maximum . Set.map getX $ ls

    minY = (\x -> x - 1) . minimum . Set.map getY $ ls
    maxY = (+ 1) . maximum . Set.map getY $ ls

    minZ = (\x -> x - 1) . minimum . Set.map getZ $ ls
    maxZ = (+ 1) . maximum . Set.map getZ $ ls

    getX (x, _, _) = x
    getY (_, y, _) = y
    getZ (_, _, z) = z

blackTiles :: [(Int, Int, Int)] -> Set (Int, Int, Int)
blackTiles = fromList . fmap fst . filter (odd . snd) . Map.toList . count

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
