{-# LANGUAGE ScopedTypeVariables #-}

module MyLib (sumsTo2020, sumsTo2020') where

import Control.Monad (guard)

sumsTo2020 :: String -> Int
sumsTo2020 input =
  let numbers = readNumbers input
   in head $ do
        a <- numbers
        b <- numbers
        guard (a + b == 2020)
        pure (a * b)

sumsTo2020' :: String -> Int
sumsTo2020' input =
  let numbers = readNumbers input
   in head $ do
        a <- numbers
        b <- numbers
        c <- numbers
        guard (a + b + c == 2020)
        pure (a * b * c)

readNumbers :: String -> [Int]
readNumbers = map read . lines