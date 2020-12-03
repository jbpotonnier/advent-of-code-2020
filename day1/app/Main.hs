module Main where

import MyLib (sumsTo2020, sumsTo2020')

main :: IO ()
main = do
  input <- readFile "./src/input.txt"
  print $ sumsTo2020 input
  print $ sumsTo2020' input
