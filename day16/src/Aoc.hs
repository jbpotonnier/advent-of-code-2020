module Aoc
  ( module Aoc,
  )
where

import Data.Maybe (fromJust)
import Text.Megaparsec
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data Notes = Notes
  { fields :: [Field],
    ticket :: Ticket,
    nearbyTickets :: [Ticket]
  }
  deriving (Show, Eq)

newtype Ticket = Ticket [Int]
  deriving (Show, Eq)

data Range = Range {start :: Int, end :: Int}
  deriving (Show, Eq)

data Field = Field {name :: Text, ranges :: [Range]}
  deriving (Show, Eq)

readInput :: FilePath -> IO (Either (ParseErrorBundle Text Void) Notes)
readInput path = parse notesP path <$> readFileText path

notesP :: Parser Notes
notesP = do
  fields <- fieldsP
  ticket <- yourTicketP
  void $ newline
  nearbyTickets <- nearbyTicketsP
  pure $ Notes fields ticket nearbyTickets
  where
    fieldsP :: Parser [Field]
    fieldsP = someTill fieldP newline

    fieldP :: Parser Field
    fieldP = do
      fieldName <- mconcat . intersperse " " <$> wordP `sepBy1` string " "
      void $ string ": "
      firstRange <- rangeP
      void $ string " or "
      secondRange <- rangeP
      void $ newline
      pure $ Field fieldName [firstRange, secondRange]

    rangeP :: Parser Range
    rangeP = do
      a <- intP
      void $ string "-"
      b <- intP
      pure $ Range a b

    yourTicketP :: Parser Ticket
    yourTicketP = do
      void $ string "your ticket:"
      void $ newline
      ticketP

    nearbyTicketsP :: Parser [Ticket]
    nearbyTicketsP = do
      void $ string "nearby tickets:"
      void $ newline
      manyTill ticketP eof

    ticketP :: Parser Ticket
    ticketP = do
      numbers <- numbersP
      void $ newline
      pure $ Ticket numbers

    numbersP :: Parser [Int]
    numbersP = intP `sepBy1` ","

-----------------------------------
wordP :: Parser Text
wordP = toText <$> Parsec.some letterChar

intP :: Parser Int
intP = readInt . toText <$> Parsec.some digitChar

-- applyTimes :: (c -> c) -> Int -> c -> c
-- applyTimes f n = (!! n) . iterate f

-- count :: (Ord k, Foldable t) => t k -> k -> Int
-- count xs x = fromMaybe 0 (c IMap.!? x)
--   where
--     c = IMap.fromListWith (+) . fmap (,1) . toList $ xs

readInt :: Text -> Int
readInt = fromJust . readMaybe . toString

-- head' :: [c] -> c
-- head' = fromJust . viaNonEmpty head

-- headMay :: [b] -> Maybe b
-- headMay = viaNonEmpty head

-- tail' :: [a] -> [a]
