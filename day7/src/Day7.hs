module Day7
  ( module Day7,
  )
where

import Data.Map.Strict ((!), (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Set (size)
import Data.Text (breakOn, splitOn, strip, stripSuffix)

type Color = Text

data Rule = Rule Color [(Integer, Color)]
  deriving (Show, Eq)

countBags :: [Rule] -> Color -> Integer
countBags rules color = go color - 1
  where
    go c =
      let xs = ruleMap ! c
       in 1
            + if null xs
              then 0
              else sum [n * go c' | (n, c') <- xs]

    ruleMap = Map.fromList [(h, xs) | Rule h xs <- rules]

countColors :: [[Color]] -> Int
countColors = size . fromList . fmap head'
  where
    head' xs = fromJust (viaNonEmpty head xs)

listOptions :: [Rule] -> Color -> [[Color]]
listOptions rules c = go [[c]]
  where
    go xs =
      let res = step xs
       in if res == xs then res else res ++ go res

    step :: [[Color]] -> [[Color]]
    step paths = do
      path@(p : _ps) <- paths
      holder <- holdersOf rules p
      pure (holder : path)

holdersOf :: [Rule] -> Color -> [Color]
holdersOf rules color = fromMaybe [] (ruleMap !? color)
  where
    ruleMap :: Map Color [Color]
    ruleMap =
      Map.fromListWith
        (++)
        [(c, [h]) | (h, c) <- concatMap possiblesHolders rules]

possiblesHolders :: Rule -> [(Color, Color)]
possiblesHolders (Rule h xs) = fmap (\(_, c) -> (h, c)) xs

parseRule :: Text -> Rule
parseRule s = Rule (stripBagSuffix container) (parseContent content)
  where
    (container, content) = parseContain . stripDot $ s
    parseContain t = case splitOn " contain " t of
      [a, b] -> (a, b)
      _ -> error $ "parseContain: " <> t

parseContent :: Text -> [(Integer, Color)]
parseContent t
  | t == "no other bags" = []
  | otherwise = fmap (parseItem . strip . stripBagSuffix) . splitOn ", " $ t
  where
    parseItem s = let (n, c) = breakOn " " s in (readInteger n, strip c)

readInteger :: Text -> Integer
readInteger = fromJust . readMaybe . toString

stripBagSuffix :: Text -> Text
stripBagSuffix = \case
  (stripSuffix " bag " -> Just s) -> s
  (stripSuffix " bag" -> Just s) -> s
  (stripSuffix " bags" -> Just s) -> s
  s -> error $ "stripBagSuffix: " <> s

stripDot :: Text -> Text
stripDot = \case
  (stripSuffix "." -> Just s) -> s
  s -> error $ "stripDot: " <> s
