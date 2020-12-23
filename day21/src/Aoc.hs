{-# LANGUAGE TupleSections #-}

module Aoc
  ( module Aoc,
  )
where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Text as T

isSolution :: Foldable t => t (Set a) -> Bool
isSolution = all (\s -> Set.size s == 1)

toSolution :: Map k (Set a) -> [(k, a)]
toSolution = Map.toList . fmap (Set.elemAt 0)

eliminate :: (Eq a, Ord k) => Map k (Set a) -> Map k (Set a)
eliminate m = foldl' eliminateAt m (Map.keys m)

eliminateAt :: (Eq a, Ord k) => Map k (Set a) -> k -> Map k (Set a)
eliminateAt m i
  | Set.size cur == 1 =
    let val = Set.elemAt 0 cur
     in Map.mapWithKey (\j s -> if i /= j then Set.filter (/= val) s else s) m
  | otherwise = m
  where
    cur = m Map.! i

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
