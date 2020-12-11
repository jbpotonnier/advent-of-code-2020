module Day11
  ( module Day11,
  )
where

import Data.Array.IArray (Array, bounds, inRange, listArray)
import qualified Data.Array.IArray as Array
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Text as Text

type Pos = (Int, Int)

type Layout = Array Pos Square

data Square = Floor | Empty | Occupied
  deriving (Enum, Bounded, Show, Eq, Ord)

converge :: Eq t => (t -> t) -> t -> t
converge f x = let res = f x in if res == x then res else converge f res

applyRules :: Layout -> Layout
applyRules a = a Array.// updates
  where
    updates =
      [ update assoc adj
        | assoc@(p, _s) <- Array.assocs a,
          let adj = squares a (adjacents p)
      ]

update :: (Pos, Square) -> [Square] -> (Pos, Square)
update initial@(p, s) adj = case s of
  Empty
    | count adj Occupied == 0 -> (p, Occupied)
    | otherwise -> initial
  Occupied
    | count adj Occupied >= 4 -> (p, Empty)
    | otherwise -> initial
  Floor -> initial

squares :: Layout -> [Pos] -> [Square]
squares a = fmap (a Array.!) . filter (inRange (bounds a))

adjacents :: Pos -> [Pos]
adjacents p = [step p d | d <- directions]

step :: Pos -> (Int, Int) -> Pos
step (x, y) (dx, dy) = (x + dx, y + dy)

directions :: [(Int, Int)]
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
    (_, (rowMaxBound, _)) = Array.bounds a

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
readInput path =
  fmap toLayout . traverse readLine . lines <$> readFileText path
  where
    toLayout :: [[Square]] -> Layout
    toLayout xs =
      listArray ((0, 0), (length xs - 1, length (head' xs) -1)) . mconcat $ xs
    readLine :: Text -> Maybe [Square]
    readLine = traverse readSquare . toString

-------------------
count :: (Ord k) => [k] -> k -> Int
count xs x = fromMaybe 0 (c Map.!? x)
  where
    c = Map.fromListWith (+) . fmap (,1) $ xs

-- readInt :: Text -> Maybe Int
-- readInt = readMaybe . toString

head' :: [a] -> a
head' = fromJust . viaNonEmpty head

-- tail' :: [a] -> [a]
-- tail' = fromJust . viaNonEmpty tail
