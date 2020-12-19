module Aoc
  ( module Aoc,
  )
where

import Data.Map ((!))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Text.Megaparsec
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char (string)

data Rule
  = Seq [Int]
  | Or [Int] [Int]
  | One Char
  deriving (Show, Eq)

type Parser = Parsec Void String

mkParser2 :: Map Int Rule -> Int -> Parser String
mkParser2 rules = go
  where
    --  0: 8 11
    --  8: 42 | 42 8
    -- 11: 42 31 | 42 11 31

    go :: Int -> Parser String
    go 0 = do
      a <- Parsec.some (try p42)
      b <- Parsec.some p31
      if length a > length b
        then pure $ mconcat a <> mconcat b
        else fail "failed"
    go ruleNumber = case rules ! ruleNumber of
      Seq ns -> sequenceP ns
      Or l1 l2 -> try (sequenceP l1) <|> sequenceP l2
      One c -> string [c]

    p42 = go 42

    p31 = go 31

    sequenceP :: [Int] -> Parser String
    sequenceP = mconcat . fmap go

mkParser :: Map Int Rule -> Int -> Parser String
mkParser rules = go
  where
    go :: Int -> Parser String
    go ruleNumber = case rules ! ruleNumber of
      Seq ns -> sequenceP ns
      Or l1 l2 -> try (sequenceP l1) <|> sequenceP l2
      One c -> string [c]

    sequenceP :: [Int] -> Parser String
    sequenceP = mconcat . fmap go

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

read' :: Read a => Text -> a
read' = fromJust . readMaybe . toString