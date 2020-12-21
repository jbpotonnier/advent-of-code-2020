module Aoc
  ( module Aoc,
  )
where

import Data.Foldable (Foldable (maximum, minimum))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Text.Show

type Image = Set (Int, Int)

data Tile = Tile {tileId :: Int, image :: Image}
  deriving (Eq)

instance Show Tile where
  show Tile {tileId, image} = 
    "Tile " <> show tileId <> ":\n" <> (toString . showImage) image

isSame :: Tile -> Tile -> Bool
isSame a b = tileId a == tileId b

search :: Map (Int, Int) (Vector Tile) -> [Map (Int, Int) Tile]
search = fmap solutionAsMap . go . elimination
  where
    go m
      | isSolution m = [m]
      | isFailure m = []
      | otherwise = foldl' (\acc x -> acc ++ (go . elimination) x) [] (nexts m)

elimination :: Map (Int, Int) (Vector Tile) -> Map (Int, Int) (Vector Tile)
elimination = converge (eliminate isSame isCompatible neighbors)

-- nexts :: Ord k => Map k (Vector a) -> [Map k (Vector a)]
nexts m = (`assign` m) <$> nextAssignations m

-- nextAssignations :: Map b (Vector a) -> [b]
nextAssignations =
  fmap fst
    . sortOn (Set.size . snd)
    . fmap (second (Set.fromList . fmap tileId . toList))
    . filter (\(_, v) -> Vector.length  v > 1)
    . Map.toList

solutionAsMap :: Map k (Vector a) -> Map k a
solutionAsMap = fmap Vector.head

assign :: Ord k => k -> Map k (Vector a) -> Map k (Vector a)
assign = Map.adjust (Vector.singleton . Vector.head)

isSolution :: Map k (Vector a) -> Bool
isSolution = all (\v -> Vector.length v == 1)

isFailure :: Map k (Vector a) -> Bool
isFailure = any (\v -> Vector.length v == 0)

eliminate ::
  Ord k =>
  (t -> t -> Bool) ->
  ((k, t) -> (k, t) -> Bool) ->
  (k -> [k]) ->
  Map k (Vector t) ->
  Map k (Vector t)
eliminate samePred isNbCompatible nbs m = foldl' (eliminateAt samePred isNbCompatible nbs) m (Map.keys m)

eliminateAt ::
  (Ord k) =>
  (t -> t -> Bool) ->
  ((k, t) -> (k, t) -> Bool) ->
  (k -> [k]) ->
  Map k (Vector t) ->
  k ->
  Map k (Vector t)
eliminateAt samePred isNbCompatible nbs m i
  -- value is assigned
  | Vector.length cur == 1 =
    let val = Vector.head cur
     in Map.mapWithKey (\j vect -> if i /= j then Vector.filter (not . samePred val) vect else vect) m
  | otherwise =
    Map.adjust (Vector.filter hasCompatibleNbs) i m
  where
    cur = m Map.! i

    hasCompatibleNbs val = all (isDirOK val) (neighborsValues m nbs i)

    isDirOK val (nbPos, vect) = any (isNbCompatible (i, val)) [(nbPos, v) | v <- toList vect]

neighborsValues :: Ord k => Map k b -> (t -> [k]) -> t -> [(k, b)]
neighborsValues m nbs i = catMaybes [(,) k <$> m Map.!? k | k <- nbs i]

------------------

isCompatible :: ((Int, Int), Tile) -> ((Int, Int), Tile) -> Bool
isCompatible (p1, t1) (p2, t2)
  | isSame t1 t2 = False
  | otherwise = case diff p2 p1 of
    (1, 0) -> isHorizCompatible t1 t2
    (-1, 0) -> isHorizCompatible t2 t1
    (0, 1) -> isVertCompatible t1 t2
    (0, -1) -> isVertCompatible t2 t1
    _ -> True

isHorizCompatible :: Tile -> Tile -> Bool
isHorizCompatible t1 t2 =
  col 9 (image t1) == col 0 (image t2)

isVertCompatible :: Tile -> Tile -> Bool
isVertCompatible t1 t2 =
  row 9 (image t1) == row 0 (image t2)

col :: (Ord b, Eq a) => a -> Set (a, b) -> Set b
col n = Set.map snd . Set.filter (\(x, _y) -> x == n)

row :: (Ord b1, Eq b2) => b2 -> Set (b1, b2) -> Set b1
row n = Set.map fst . Set.filter (\(_x, y) -> y == n)

diff :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
diff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

------------------

allPossibilities :: Int -> [Tile] -> Map (Int, Int) (Vector Tile)
allPossibilities probSize tiles =
  fromList [((x, y), possibles) | x <- [0 .. probSize - 1], y <- [0 .. probSize - 1]]
  where
    possibles = mconcat . map allTransformations $ tiles

allTransformations :: Tile -> Vector Tile
allTransformations tile =
  fromList [tile {image = g (image tile)} | g <- transformations]
  where
    transformations = do
      a <- [id, flipX, flipY]
      c <- [id, rotate, rotate . rotate, rotate . rotate . rotate]
      pure $ a . c

flipX :: Image -> Image
flipX = Set.map (first (9 -))

flipY :: Image -> Image
flipY = Set.map (second (9 -))

rotate :: Image -> Image
rotate = Set.map (\(x, y) -> (9 - y, x))

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + dx, y + dy) | (dx, dy) <- directions]
  where
    directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]

------------------

showImage :: Image -> Text
showImage image =
  unlines
    [ mconcat
        [showPixel (isActive (x, y) image) | x <- [minX .. maxX]]
      | y <- [minY .. maxY]
    ]
  where
    minX = minimum . Set.map fst $ image
    maxX = maximum . Set.map fst $ image

    minY = minimum . Set.map snd $ image
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
        . fmap (\(y, xs) -> [(x, y) | x <- xs])
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

------------------

converge :: Eq t => (t -> t) -> t -> t
converge f x = let res = f x in if res == x then res else converge f res

read' :: Read a => String -> a
read' = fromJust . readMaybe

head' :: [y] -> y
head' = fromJust . viaNonEmpty head
