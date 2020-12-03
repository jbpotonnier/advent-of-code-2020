module Main where

import MyLib (checkPassword, checkPassword2, readLine)

main :: IO ()
main = do
  content <- readFile "./app/input.txt"
  let policiesAndPassword = map readLine . lines $ content
  case sequence policiesAndPassword of
    Left e -> error e
    Right policiesAndPassword' -> do
      print $ countOK checkPassword policiesAndPassword'
      print $ countOK checkPassword2 policiesAndPassword'

countOK :: (a -> b -> Bool) -> [(a, b)] -> Int
countOK check = length . filter (== True) . fmap (uncurry check)