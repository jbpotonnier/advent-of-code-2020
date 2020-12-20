module Aoc
  ( module Aoc,
  )
where

import Data.Foldable (Foldable (maximum))
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.Show

type Image = Set (Int, Int)

data Tile = Tile {tileId :: Int, image :: Image}
  deriving (Eq)

instance Show Tile where
  show Tile {tileId, image} =
    "Tile " <> show tileId <> ":\n" <> (toString . showImage) image

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + dx, y + dy) | (dx, dy) <- directions]

directions :: [(Int, Int)]
directions =
  [d | let ds = [-1, 0, 1], dx <- ds, dy <- ds, let d = (dx, dy), d /= (0, 0)]

showImage :: Image -> Text
showImage image =
  unlines
    [ mconcat
        [showPixel (isActive (x, y) image) | y <- [0 .. maxY]]
      | x <- [0 .. maxX]
    ]
  where
    maxX = maximum . Set.map fst $ image
    maxY = maximum . Set.map snd $ image
    isActive p coords = p `Set.member` coords
    showPixel = bool "." "#"

readInput :: MonadIO f => FilePath -> f [Tile]
readInput path = fmap readTile . T.splitOn "\n\n" <$> readFileText path

readTile :: Text -> Tile
readTile t = case lines t of
  firstLine : imageLines -> Tile (readTileId firstLine) (readImage imageLines)
  _ -> error $ "readTile cannot read tile: " <> show t
  where
    readTileId l = read' $ case words l of
      ["Tile", s] -> toString . fromJust . T.stripSuffix ":" $ s
      _ -> error $ "cannot read tile id: " <> show l

readImage :: [Text] -> Image
readImage = toImage . fmap (readLine . toString)
  where
    toImage :: [[Int]] -> Image
    toImage =
      fromList
        . mconcat
        . fmap (\(y, xs) -> [(y, x) | x <- xs])
        . enumerate

    readLine :: String -> [Int]
    readLine =
      fmap fst
        . filter snd
        . enumerate
        . fmap readPixel

    readPixel :: Char -> Bool
    readPixel = \case
      '.' -> False
      '#' -> True
      p -> error $ "readPixel: cannot read " <> show p

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

-----------------------------------

-- converge :: Eq t => (t -> t) -> t -> t
-- converge f x = let res = f x in if res == x then res else converge f res

-- applyTimes :: (y -> y) -> Int -> y -> y
-- applyTimes f n = (!! n) . iterate f

-- count :: (Ord k, Foldable t) => t k -> k -> Int
-- count xs x = fromMaybe 0 (y IMap.!? x)
--   where
--     y = IMap.fromListWith (+) . fmap (,1) . toList $ xs

read' :: Read a => String -> a
read' = fromJust . readMaybe

-- head' :: [y] -> y
-- head' = fromJust . viaNonEmpty head

-- headMay :: [b] -> Maybe b
-- headMay = viaNonEmpty head

-- tail' :: [a] -> [a]

-- search :: Eq a => Vector (Vector a) -> Vector (Vector a)
-- search vs
--  | isSolution vs = vs
--  | otherwise =
--    let candidateIndex = V.minIndexBy (comparing V.length) vs
--     in search . eliminate $ assign vs candidateIndex

-- assign :: Vector (Vector a) -> Int -> Vector (Vector a)
-- assign vs i = vs V.// [(i, V.singleton (V.head (vs V.! i)))]

-- solutionAsList :: Vector (Vector a) -> [a]
-- solutionAsList = toList . V.map V.head

-- isSolution :: Vector (Vector a) -> Bool
-- isSolution = all (\v -> V.length v == 1)

-- eliminate :: Eq a => Vector (Vector a) -> Vector (Vector a)
-- eliminate v = foldl' eliminateAt v [0 .. V.length v - 1]

-- eliminateAt :: Eq a => Vector (Vector a) -> Int -> Vector (Vector a)
-- eliminateAt vs i
--   | V.length cur == 1 =
--     let val = V.head cur
--      in V.imap (\j v -> if i /= j then V.filter (/= val) v else v) vs
--   | otherwise = vs
--   where
--     cur = vs V.! i
