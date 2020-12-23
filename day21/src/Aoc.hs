{-# LANGUAGE TupleSections #-}

module Aoc
  ( module Aoc,
  )
where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Text as T

inerts :: ([Text], Map Text (Set Text)) -> [Text]
inerts (ingredients, m) =
  let withoutAllergen = fromList ingredients `Set.difference` (mconcat . Map.elems $ m)
   in filter (`Set.member` withoutAllergen) ingredients

readInput :: MonadIO f => FilePath -> f ([Text], Map Text (Set Text))
readInput path = do
  ls <- fmap readLine . lines <$> readFileText path
  let ingredients = mconcat . fmap fst $ ls
      m =
        Map.fromListWith Set.intersection
          . mconcat
          . fmap (\(is, als) -> fmap (second Set.fromList . (,is)) als)
          $ ls
  pure (ingredients, m)

readLine :: Text -> ([Text], [Text])
readLine =
  bimap words (T.splitOn ", " . stripSuffix ")")
    . splitOn "(contains "

splitOn :: Text -> Text -> (Text, Text)
splitOn s t = case T.splitOn s t of
  [a, b] -> (a, b)
  _ -> error $ "splitOn: cannot split " <> show t <> " on " <> show s

stripSuffix :: Text -> Text -> Text
stripSuffix s = fromJust . T.stripSuffix s

converge :: Eq t => (t -> t) -> t -> t
converge f x = let res = f x in if res == x then res else converge f res

-----------------------------------

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
