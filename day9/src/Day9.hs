module Day9
  ( module Day9,
  )
where

import Data.List.Split (divvy)

findContiguousSet :: [Int] -> Int -> [[Int]]
findContiguousSet numbers invalidNumber =
  filter (\s -> sum s == invalidNumber) $ subLists numbers

subLists :: [a] -> [[a]]
subLists xs = mconcat [divvy s 1 xs | s <- [2 .. length xs]]

findFirstInvalid :: Int -> [Int] -> Int
findFirstInvalid preambleSize numbers = go preamble rest
  where
    go p@(_ : ps) (n : ns)
      | validate p n = go (ps ++ [n]) ns
      | otherwise = n
    go p ns = error $ "invalid input for go" <> show (p, ns)

    (preamble, rest) = splitAt preambleSize numbers

validate :: [Int] -> Int -> Bool
validate preamble n = or [n == a + b | a <- preamble, b <- preamble, a /= b]

readInput :: FilePath -> IO (Maybe [Int])
readInput path =
  traverse readInt . lines <$> readFileText path

readInt :: Text -> Maybe Int
readInt = readMaybe . toString
