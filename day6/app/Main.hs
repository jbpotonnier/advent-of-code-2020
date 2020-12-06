{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable (Foldable (foldl1))
import Data.Set (intersection, size)
import Data.Text (splitOn)

firstStar :: Text -> Int
firstStar = run (toSet . mconcat)

secondStar :: Text -> Int
secondStar = run (foldl1 intersection . fmap toSet)

run :: ([Text] -> Set a) -> Text -> Int
run f = sum . fmap (size . f . lines) . splitOn "\n\n"

toSet :: Text -> Set Char
toSet = fromList . toString

main :: IO ()
main = do
  example <- readFileText "./test/example.txt"
  content <- readFileText "./test/input.txt"
  let inputs = [example, content]
  print $ fmap firstStar inputs == [11, 6885]
  print $ fmap secondStar inputs == [6, 3550]
