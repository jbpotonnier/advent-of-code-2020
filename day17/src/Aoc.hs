module Aoc
  ( module Aoc,
  )
where

import Data.List (maximum, minimum)
import qualified Data.Set as Set

hyperTransform :: Set (Int, Int, Int, Int) -> Set (Int, Int, Int, Int)
hyperTransform coords =
  fromList
    [ (r, c, z, w)
      | r <- range rowBounds,
        c <- range colBounds,
        z <- range zBounds,
        w <- range wBounds,
        computeActivity (r, c, z, w)
    ]
  where
    computeActivity c =
      let activeNeighborsCount = countNeighbors c
       in if isActive c coords
            then activeNeighborsCount == 2 || activeNeighborsCount == 3
            else activeNeighborsCount == 3

    countNeighbors = length . filter (`isActive` coords) . hyperNeighbors

    range (s, e) = [s - 1 .. e + 1]

    rowBounds =
      let rs = Set.map (\(r, _, _, _) -> r) coords
       in (minimum rs, maximum rs)

    colBounds =
      let cs = Set.map (\(_, c, _, _) -> c) coords
       in (minimum cs, maximum cs)

    zBounds =
      let zs = Set.map (\(_, _, z, _) -> z) coords
       in (minimum zs, maximum zs)

    wBounds =
      let ws = Set.map (\(_, _, w, _) -> w) coords
       in (minimum ws, maximum ws)

hyperNeighbors :: (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]
hyperNeighbors (r, c, z, w) =
  [(r + dr, c + dc, z + dz, w + dw) | (dr, dc, dz, dw) <- hyperDirections]

hyperDirections :: [(Int, Int, Int, Int)]
hyperDirections =
  [ d
    | let ds = [-1, 0, 1],
      dr <- ds,
      dc <- ds,
      dz <- ds,
      dw <- ds,
      let d = (dr, dc, dz, dw),
      d /= (0, 0, 0, 0)
  ]

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

isActive :: Ord a => a -> Set a -> Bool
isActive p coords = p `Set.member` coords

countActive :: Set a -> Int
countActive = Set.size

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

readHyperSpace :: Text -> Set (Int, Int, Int, Int)
readHyperSpace = toHyperSpace . readLines

readSpace :: Text -> Set (Int, Int, Int)
readSpace = toSpace . readLines

readLines :: Text -> [[Int]]
readLines = fmap readLine . lines

toHyperSpace :: [[Int]] -> Set (Int, Int, Int, Int)
toHyperSpace =
  fromList
    . mconcat
    . fmap (\(i, r) -> [(i, c, 0, 0) | c <- r])
    . enumerate

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
