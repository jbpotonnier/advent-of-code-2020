module Day10
  ( module Day10,
  )
where

import Data.Foldable (Foldable (maximum))
import qualified Data.Map as Map
import Data.Maybe (fromJust)

star2 :: [Int] -> [[Int]]
star2 xs = dump . cut . sort $ xs'
  where
    xs' = (0 : fromList (sort xs)) ++ [maximum xs + 3]

data Cut = Cut Int [Cut] | Empty
  deriving (Show)

dump :: Cut -> [[Int]]
dump Empty = []
dump (Cut i []) = [[i]]
dump (Cut i cuts) = map ([i] ++) (concatMap dump cuts)

cut :: [Int] -> Cut
cut [] = Empty
cut ys@(x : xs) =
  let cs = [x' | x' <- xs, x' - x <= 3]
      cuts = [cut (dropWhile (< c) ys) | c <- cs]
   in Cut x cuts

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
