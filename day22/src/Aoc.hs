module Aoc
  ( module Aoc,
  )
where

import Data.Maybe (fromJust)
import Data.Sequence (Seq (Empty, (:<|)))
import qualified Data.Sequence as Seq

data Game = Game
  { player1 :: Seq Int,
    player2 :: Seq Int
  }
  deriving (Show)

play :: Game -> Game
play = head' . dropWhile (not . isFinished) . iterate step
  where
    isFinished :: Game -> Bool
    isFinished Game {player1, player2} = Seq.null player1 || Seq.null player2

    step :: Game -> Game
    step game@Game {player1, player2} =
      case (player1, player2) of
        (Empty, _) -> game
        (_, Empty) -> game
        (c1 :<| c1s, c2 :<| c2s)
          | c1 > c2 ->
            Game
              { player1 = c1s <> fromList [c1, c2],
                player2 = c2s
              }
          | otherwise ->
            Game
              { player1 = c1s,
                player2 = c2s <> fromList [c2, c1]
              }

score :: Game -> Int
score game@Game {player1, player2} =
  case (player1, player2) of
    (Empty, cs) -> compute cs
    (cs, Empty) -> compute cs
    _ -> error $ "score: game is not finished " <> show game
  where
    compute = sum . fmap (uncurry (*)) . enumerate . Seq.reverse

enumerate :: Seq b -> Seq (Int, b)
enumerate s = Seq.zip (fromList [1 .. Seq.length s]) s

-----------------------------------

-- applyTimes :: (c -> c) -> Int -> c -> c
-- applyTimes f n = (!! n) . iterate f

-- count :: (Ord k, Foldable t) => t k -> k -> Int
-- count xs x = fromMaybe 0 (c IMap.!? x)
--   where
--     c = IMap.fromListWith (+) . fmap (,1) . toList $ xs

-- readInt :: Text -> Maybe Int
-- readInt = readMaybe . toString

head' :: [c] -> c
head' = fromJust . viaNonEmpty head

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