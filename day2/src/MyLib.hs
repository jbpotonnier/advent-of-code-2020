module MyLib
  ( Policy (..),
    readPolicy,
    readLine,
    checkPassword,
    checkPassword2,
  )
where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Safe (atMay)
import Text.Read (readEither)

data Policy = Policy Int Int Char
  deriving (Eq, Show)

checkPassword :: Policy -> String -> Bool
checkPassword (Policy minCount maxCount c) p =
  minCount <= count && count <= maxCount
  where
    count = countElem c p

checkPassword2 :: Policy -> String -> Bool
checkPassword2 (Policy firstPos secondPos c) p =
  countElem c chars == 1
  where
    chars = [firstPos, secondPos] <&> (\pos -> atMay p (pos - 1)) & catMaybes

countElem :: Eq a => a -> [a] -> Int
countElem c = length . filter (== c)

readLine :: String -> Either String (Policy, String)
readLine s =
  let [policyString, password] = splitOn ": " s
   in (,) <$> readPolicy policyString <*> pure password

readPolicy :: String -> Either String Policy
readPolicy s =
  let [countString, [c]] = words s
      [minCountString, maxCountString] = splitOn "-" countString
   in Policy
        <$> readEither minCountString
        <*> readEither maxCountString
        <*> pure c
