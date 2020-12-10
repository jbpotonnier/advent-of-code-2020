module Day10
  ( module Day10,
  )
where

import Data.Foldable (Foldable (maximum))
import Data.Function.Memoize (memoFix)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

star2 :: [Int] -> Int
star2 xs = cut . sort $ xs'
  where
    xs' = (0 : fromList (sort xs)) ++ [maximum xs + 3]

cut :: [Int] -> Int
cut = memoFix cut'

cut' :: (Num p, Num a, Ord a) => ([a] -> p) -> [a] -> p
cut' _ [] = 0
cut' f ys@(x : xs) =
  let cs = [x' | x' <- take 4 xs, x' - x <= 3]
      cuts = [f (dropWhile (< c) ys) | c <- cs]
   in if null cuts then 1 else sum cuts

differences :: [Int] -> [Int]
differences xs = zipWith (flip (-)) xs' (tail' xs')
  where
    xs' = (0 : fromList (sort xs)) ++ [maximum xs + 3]

count :: (Ord k) => [k] -> Map k Int
count = Map.fromListWith (+) . fmap (,1)

-------------------

readInput :: Applicative f => (Text -> f b) -> FilePath -> IO (f [b])
readInput readLine path =
  traverse readLine . lines <$> readFileText path

readInt :: Text -> Maybe Int
readInt = readMaybe . toString

head' :: [a] -> a
head' = fromJust . viaNonEmpty head

tail' :: [a] -> [a]
tail' = fromJust . viaNonEmpty tail
