module Day11
  ( module Day11,
  )
where

import Data.Array.IArray (Array, IArray, Ix, bounds, inRange, listArray)
import qualified Data.Array.IArray as Array
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Text as Text

type Pos = (Int, Int)

type Dir = (Int, Int)

type Layout = Array Pos Square

data Square = Floor | Empty | Occupied
  deriving (Enum, Bounded, Show, Eq, Ord)

converge :: Eq t => (t -> t) -> t -> t
converge f x = let res = f x in if res == x then res else converge f res

applyRules :: Layout -> Layout
applyRules a = a Array.// updates
  where
    updates =
      [update 4 assoc (squares a (adjacents p)) | assoc@(p, _s) <- Array.assocs a]

applyRules2 :: Layout -> Layout
applyRules2 a = a Array.// updates
  where
    updates =
      [update 5 assoc (sees a p) | assoc@(p, _s) <- Array.assocs a]

update :: Int -> (Pos, Square) -> [Square] -> (Pos, Square)
update no initial@(p, s) ss = case s of
  Empty
    | count ss Occupied == 0 -> (p, Occupied)
    | otherwise -> initial
  Occupied
    | count ss Occupied >= no -> (p, Empty)
    | otherwise -> initial
  Floor -> initial

squares :: Layout -> [Pos] -> [Square]
squares a = fmap (a Array.!) . filter (inArray a)

sees :: Layout -> Pos -> [Square]
sees a pos = squares a . catMaybes $ [walk d continue pos | d <- directions]
  where
    continue p = inArray a p && isTansparent p

    isTansparent p = case (a :: Layout) Array.! p of
      Occupied -> False
      Empty -> False
      Floor -> True

walk :: Dir -> (Pos -> Bool) -> Pos -> Maybe Pos
walk d continue = headMay . dropWhile continue . drop 1 . iterate (step d)

adjacents :: Pos -> [Pos]
adjacents p = [step d p | d <- directions]

step :: Dir -> Pos -> Pos
step (dx, dy) (x, y) = (x + dx, y + dy)

directions :: [Dir]
directions =
  [ d
    | let ds = [-1, 0, 1],
      dx <- ds,
      dy <- ds,
      let d = (dx, dy),
      d /= (0, 0)
  ]

showLayout :: Layout -> Text
showLayout a = toLines . chunksOf (rowMaxBound + 1) . fmap showSquare . Array.elems $ a
  where
    toLines = Text.unlines . fmap mconcat
    (_, (_, rowMaxBound)) = Array.bounds a

showSquare :: Square -> Text
showSquare = \case
  Floor -> "."
  Empty -> "L"
  Occupied -> "#"

readSquare :: Char -> Maybe Square
readSquare = \case
  '.' -> Just Floor
  'L' -> Just Empty
  '#' -> Just Occupied
  _ -> Nothing

readInput :: FilePath -> IO (Maybe Layout)
readInput path = do
  text <- readFileText path
  pure $ readLayout text

readLayout :: Text -> Maybe Layout
readLayout = fmap toLayout . traverse readLine . lines
  where
    toLayout :: [[Square]] -> Layout
    toLayout xs =
      listArray ((0, 0), (length xs - 1, length (head' xs) -1)) . mconcat $ xs
    readLine :: Text -> Maybe [Square]
    readLine = traverse readSquare . toString

inArray :: (Ix a1, IArray a2 e) => a2 a1 e -> a1 -> Bool
inArray a = inRange (bounds a)

-------------------
count :: (Ord k, Foldable t) => t k -> k -> Int
count xs x = fromMaybe 0 (c Map.!? x)
  where
    c = Map.fromListWith (+) . fmap (,1) . toList $ xs

-- readInt :: Text -> Maybe Int
-- readInt = readMaybe . toString

head' :: [c] -> c
head' = fromJust . viaNonEmpty head

headMay :: [b] -> Maybe b
headMay = viaNonEmpty head

-- tail' :: [a] -> [a]
-- tail' = fromJust . viaNonEmpty tail
