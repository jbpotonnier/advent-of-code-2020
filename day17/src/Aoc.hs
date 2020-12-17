module Aoc
  ( module Aoc,
  )
where

import Data.List (maximum, minimum)
import qualified Data.Set as Set

countActive :: Set a -> Int
countActive = Set.size

transform :: Set (Int, Int, Int) -> Set (Int, Int, Int)
transform coords =
  fromList
    [ (r, c, z)
      | r <- range rowBounds,
        c <- range colBounds,
        z <- range zBounds,
        computeActivity (r, c, z)
    ]
  where
    computeActivity c =
      let activeNeighborsCount = countNeighbors c
       in if isActive c coords
            then activeNeighborsCount == 2 || activeNeighborsCount == 3
            else activeNeighborsCount == 3

    countNeighbors = length . filter (`isActive` coords) . neighbors

    range (s, e) = [s - 1 .. e + 1]

    rowBounds =
      let rs = Set.map (\(r, _, _) -> r) coords
       in (minimum rs, maximum rs)

    colBounds =
      let cs = Set.map (\(_, c, _) -> c) coords
       in (minimum cs, maximum cs)

    zBounds =
      let zs = Set.map (\(_, _, z) -> z) coords
       in (minimum zs, maximum zs)

isActive :: Ord a => a -> Set a -> Bool
isActive p coords = p `Set.member` coords

neighbors :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbors (r, c, z) = [(r + dr, c + dc, z + dz) | (dr, dc, dz) <- directions]

directions :: [(Int, Int, Int)]
directions =
  [ d
    | let ds = [-1, 0, 1],
      dr <- ds,
      dc <- ds,
      dz <- ds,
      let d = (dr, dc, dz),
      d /= (0, 0, 0)
  ]

showLayer :: Set (Int, Int) -> Text
showLayer coords =
  unlines
    [ mconcat [showCube (isActive (r, c) coords) | c <- [0 .. maxCol]]
      | r <- [0 .. maxRow]
    ]
  where
    maxRow = maximum . Set.map fst $ coords
    maxCol = maximum . Set.map snd $ coords

layer :: Int -> Set (Int, Int, Int) -> Set (Int, Int)
layer zIndex =
  Set.map (\(x, y, _) -> (x, y))
    . Set.filter (\(_, _, z) -> z == zIndex)

readSpace :: Text -> Set (Int, Int, Int)
readSpace = toSpace . fmap readLine . lines
  where
    toSpace :: [[Int]] -> Set (Int, Int, Int)
    toSpace =
      fromList
        . mconcat
        . fmap (\(i, r) -> [(i, c, 0) | c <- r])
        . enumerate

    readLine :: Text -> [Int]
    readLine =
      fmap fst
        . filter snd
        . enumerate
        . mapMaybe readCube
        . toString

showCube :: Bool -> Text
showCube = \case
  False -> "."
  True -> "#"

readCube :: Char -> Maybe Bool
readCube = \case
  '.' -> Just False
  '#' -> Just True
  _ -> Nothing

-----------------------------------

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0 ..]

-- applyTimes :: (c -> c) -> Int -> c -> c
-- applyTimes f n = (!! n) . iterate f

-- count :: (Ord k, Foldable t) => t k -> k -> Int
-- count xs x = fromMaybe 0 (c IMap.!? x)
--   where
--     c = IMap.fromListWith (+) . fmap (,1) . toList $ xs

-- readInt :: Text -> Maybe Int
-- readInt = readMaybe . toString

-- head' :: [c] -> c
-- head' = fromJust . viaNonEmpty head

-- headMay :: [b] -> Maybe b
-- headMay = viaNonEmpty head

-- tail' :: [a] -> [a]
