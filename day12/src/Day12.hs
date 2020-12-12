module Day12
  ( module Day12,
  )
where

import Data.List ((!!))

data Action = N Int | S Int | E Int | W Int | L Int | R Int | F Int
  deriving (Show, Eq)

type Pos = (Int, Int)

data World2
  = World2
      Pos -- ship
      Pos -- waypoint
  deriving (Show)

data World = World Pos Int
  deriving (Show)

manhattan :: Pos -> Pos -> Int
manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

compute2 :: World2 -> [Action] -> World2
compute2 = foldl' next2

next2 :: World2 -> Action -> World2
next2 (World2 ship waypoint@(wpx, wpy)) = \case
  N n -> World2 ship (wpx, wpy + n)
  S n -> World2 ship (wpx, wpy - n)
  E n -> World2 ship (wpx + n, wpy)
  W n -> World2 ship (wpx - n, wpy)
  L n -> World2 ship (rotate waypoint (negate (n `mod` (-360))))
  R n -> World2 ship (rotate waypoint n)
  F n -> World2 (applyTimes (move waypoint) n ship) waypoint

rotate :: Pos -> Int -> Pos
rotate (x, y) = \case
  90 -> (y, - x)
  180 -> (- x, - y)
  270 -> (- y, x)
  o -> error $ "Bad rotate:" <> show o

move :: Pos -> Pos -> Pos
move (dx, dy) (x, y) = (x + dx, y + dy)

compute :: World -> [Action] -> World
compute = foldl' next

next :: World -> Action -> World
next w@(World p@(x, y) d) = \case
  N n -> World (x, y + n) d
  S n -> World (x, y - n) d
  E n -> World (x + n, y) d
  W n -> World (x - n, y) d
  L n -> World p (nextDirection d (negate n))
  R n -> World p (nextDirection d n)
  F n -> next w (forwardAsAction d n)

forwardAsAction :: Int -> Int -> Action
forwardAsAction d n = case d of
  0 -> N n
  90 -> E n
  180 -> S n
  270 -> W n
  o -> error $ "Bad direction:" <> show o

nextDirection :: Int -> Int -> Int
nextDirection d n = (d + n) `mod` 360

readInput :: FilePath -> IO [Action]
readInput path = mapMaybe readLine . lines <$> readFileText path
  where
    readLine =
      toString >>> \case
        'N' : cs -> N <$> readMaybe cs
        'S' : cs -> S <$> readMaybe cs
        'E' : cs -> E <$> readMaybe cs
        'W' : cs -> W <$> readMaybe cs
        'L' : cs -> L <$> readMaybe cs
        'R' : cs -> R <$> readMaybe cs
        'F' : cs -> F <$> readMaybe cs
        _ -> Nothing

-----------------------------------

applyTimes :: (c -> c) -> Int -> c -> c
applyTimes f n = (!! n) . iterate f

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
