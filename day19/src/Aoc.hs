module Aoc
  ( module Aoc,
  )
where

import Data.Map ((!))
import Data.Maybe (fromJust)
import qualified Data.Text as T

data Rule
  = Seq [Int]
  | Or [Int] [Int]
  | One Char
  deriving (Show, Eq)

possibles :: Map Int Rule -> Int -> Set String
possibles rules = fromList . go
  where
    go :: Int -> [String]
    go ruleNumber =
      case rules ! ruleNumber of
        Seq ns -> fmap mconcat . traverse go $ ns
        Or l1 l2 ->
          (fmap mconcat . traverse go $ l1) <> (fmap mconcat . traverse go $ l2)
        One c -> [[c]]

readInput :: MonadIO m => FilePath -> m (Map Int Rule, [String])
readInput path = do
  content <- readFileText path
  let (rulesText, messagesText) = splitOn "\n\n" content
  pure (parseRules rulesText, parseMessages messagesText)

parseRules :: Text -> Map Int Rule
parseRules = fromList . fmap parseLine . lines
  where
    parseLine :: Text -> (Int, Rule)
    parseLine = bimap read' parseRule . splitOn ": "

    parseRule :: Text -> Rule
    parseRule t
      | ruleSep `T.isInfixOf` t = parseOr t
      | "\"" `T.isPrefixOf` t = parseOne t
      | otherwise = parseSeq t

    parseOr :: Text -> Rule
    parseOr t =
      let (f, s) = splitOn ruleSep t
       in Or (readList f) (readList s)

    parseOne :: Text -> Rule
    parseOne =
      toString
        >>> ( \case
                ['"', c, '"'] -> One c
                o -> error $ "parseOne: cannot parse " <> show o
            )

    parseSeq :: Text -> Rule
    parseSeq = Seq . readList

    readList :: Text -> [Int]
    readList = fmap read' . T.splitOn " "

    ruleSep = " | "

parseMessages :: Text -> [String]
parseMessages = fmap toString . lines

-----------------------------------

splitOn :: Text -> Text -> (Text, Text)
splitOn s t = case T.splitOn s t of
  [a, b] -> (a, b)
  _ -> error $ "splitOn: cannot split " <> show t <> " on " <> show s

-- converge :: Eq t => (t -> t) -> t -> t
-- converge f x = let res = f x in if res == x then res else converge f res

-- applyTimes :: (c -> c) -> Int -> c -> c
-- applyTimes f n = (!! n) . iterate f

-- count :: (Ord k, Foldable t) => t k -> k -> Int
-- count xs x = fromMaybe 0 (c IMap.!? x)
--   where
--     c = IMap.fromListWith (+) . fmap (,1) . toList $ xs

read' :: Read a => Text -> a
read' = fromJust . readMaybe . toString

-- head' :: [c] -> c
-- head' = fromJust . viaNonEmpty head

-- headMay :: [b] -> Maybe b
-- headMay = viaNonEmpty head

-- tail' :: [a] -> [a]

-- search :: Eq a => Vector (Vector a) -> Vector (Vector a)
-- search vs
--  | isSolution vs = vs
--  | otherwise =
--    let candidateIndex = V.minIndexBy (comparing V.length) vs
--     in search . eliminate $ assign vs candidateIndex

-- assign :: Vector (Vector a) -> Int -> Vector (Vector a)
-- assign vs i = vs V.// [(i, V.singleton (V.head (vs V.! i)))]

-- solutionAsList :: Vector (Vector a) -> [a]
-- solutionAsList = toList . V.map V.head

-- isSolution :: Vector (Vector a) -> Bool
-- isSolution = all (\v -> V.length v == 1)

-- eliminate :: Eq a => Vector (Vector a) -> Vector (Vector a)
-- eliminate v = foldl' eliminateAt v [0 .. V.length v - 1]

-- eliminateAt :: Eq a => Vector (Vector a) -> Int -> Vector (Vector a)
-- eliminateAt vs i
--   | V.length cur == 1 =
--     let val = V.head cur
--      in V.imap (\j v -> if i /= j then V.filter (/= val) v else v) vs
--   | otherwise = vs
--   where
--     cur = vs V.! i
