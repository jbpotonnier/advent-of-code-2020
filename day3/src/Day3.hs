{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day3
  ( readLine,
    Square (..),
    Grid (..),
    Direction (..),
    Step (..),
    readSquare,
    makeGrid,
    readGrid,
    path,
  )
where

data Square = Open | Tree
  deriving (Show, Eq)

newtype Grid = Grid {unGrid :: [[Square]]}

data Direction
  = DirectionRight Int
  | DirectionDown Int

newtype Step = Step [Direction]

type Position = (Int, Int)

path :: Grid -> Step -> [Square]
path g = catMaybes . takeWhile isJust . fmap (gridElem g) . positions

positions ::
  Step ->
  -- | infinite list of positions
  [Position]
positions s = iterate (step s) initPos
  where
    initPos = step s (0, 0) -- ignore the value at (0,0)

gridElem :: Grid -> Position -> Maybe Square
gridElem (Grid g) (row, col) = do
  rowElems <- g !!? row
  rowElems !!? col

step :: Step -> Position -> Position
step (Step directions) pos = foldr nextPosition pos directions
  where
    nextPosition d (row, col) =
      case d of
        DirectionRight n -> (row, col + n)
        DirectionDown m -> (row + m, col)

readGrid :: FilePath -> IO Grid
readGrid = fmap textToGrid . readFileText
  where
    textToGrid = makeGrid . readSquare

makeGrid :: [[Square]] -> Grid
makeGrid = Grid . fmap cycle -- make infinite Grid

readSquare :: Text -> [[Square]]
readSquare = fmap readLine . lines

readLine :: Text -> [Square]
readLine = fmap readChar . toString
  where
    readChar = \case
      '.' -> Open
      '#' -> Tree
      c -> error $ "Cannot read char" <> show c