{-# LANGUAGE NamedFieldPuns #-}

module Aoc
  ( module Aoc,
  )
where

import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Megaparsec
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char
import qualified Text.Show

type Parser = Parsec Void Text

data Notes = Notes
  { fields :: [Field],
    ticket :: Ticket,
    nearbyTickets :: [Ticket]
  }
  deriving (Show, Eq)

data Range = Range {start :: Int, end :: Int}
  deriving (Eq)

instance Show Range where
  show Range {start, end} = mconcat ["[", show start, "-", show end, "]"]

data Field = Field {name :: Text, ranges :: [Range]}
  deriving (Eq)

instance Show Field where
  show Field {name, ranges} = "<" <> show name <> " " <> show ranges <> ">"

type Ticket = Vector Int

findFieldOrdersInNotes :: Notes -> [Field]
findFieldOrdersInNotes note@Notes {fields} =
  findFieldOrders fields (validTicketsInNotes note)

findFieldOrders :: [Field] -> [Ticket] -> [Field]
findFieldOrders fields validTickets =
  solutionAsList . search . eliminate $ possible fields validTickets

search :: Eq a => Vector (Vector a) -> Vector (Vector a)
search vs
  | isSolution vs = vs
  | otherwise =
    let candidateIndex = V.minIndexBy (comparing V.length) vs
     in search . eliminate $ assign vs candidateIndex

assign :: Vector (Vector a) -> Int -> Vector (Vector a)
assign vs i = vs V.// [(i, V.singleton (V.head (vs V.! i)))]

solutionAsList :: Vector (Vector a) -> [a]
solutionAsList = toList . V.map V.head

isSolution :: Vector (Vector a) -> Bool
isSolution = all (\v -> V.length v == 1)

eliminate :: Eq a => Vector (Vector a) -> Vector (Vector a)
eliminate v = foldl' eliminateAt v [0 .. V.length v - 1]

eliminateAt :: Eq a => Vector (Vector a) -> Int -> Vector (Vector a)
eliminateAt vs i
  | V.length cur == 1 =
    let val = V.head cur
     in V.imap (\j v -> if i /= j then V.filter (/= val) v else v) vs
  | otherwise = vs
  where
    cur = vs V.! i

possible :: [Field] -> [Ticket] -> Vector (Vector Field)
possible fields validTickets = fromList [validFields i | i <- [0 .. length fields - 1]]
  where
    validFields i = V.fromList [f | f <- fields, isFieldValidAt f i validTickets]

isFieldValidAt :: Field -> Int -> [Ticket] -> Bool
isFieldValidAt field index validTickets =
  all (isFieldValid field) (ticketValuesAt validTickets index)

ticketValuesAt :: [Ticket] -> Int -> [Int]
ticketValuesAt tickets i = (V.! i) <$> tickets

validTicketsInNotes :: Notes -> [Ticket]
validTicketsInNotes Notes {fields, nearbyTickets} =
  filter isTicketValid nearbyTickets
  where
    isTicketValid t = null (findInvalid fields t)

--------------------------------------------

findInvalidInNotes :: Notes -> [Int]
findInvalidInNotes Notes {fields, nearbyTickets} =
  concatMap (findInvalid fields) nearbyTickets

isInRange :: Range -> Int -> Bool
isInRange Range {start, end} n = start <= n && n <= end

findInvalid :: [Field] -> Ticket -> [Int]
findInvalid fields = filter (not . isValid) . toList
  where
    isValid n = or [isFieldValid f n | f <- fields]

isFieldValid :: Field -> Int -> Bool
isFieldValid Field {ranges} n = or [isInRange r n | r <- ranges]

---------------------------------------

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0 ..]

readInput :: FilePath -> IO (Either (ParseErrorBundle Text Void) Notes)
readInput path = parse notesP path <$> readFileText path

notesP :: Parser Notes
notesP = do
  fields <- fieldsP
  ticket <- yourTicketP
  void newline
  nearbyTickets <- fromList <$> nearbyTicketsP
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
      void newline
      pure $ Field fieldName [firstRange, secondRange]

    rangeP :: Parser Range
    rangeP = do
      a <- intP
      void $ string "-"
      Range a <$> intP

    yourTicketP :: Parser Ticket
    yourTicketP = do
      void $ string "your ticket:"
      void newline
      ticketP

    nearbyTicketsP :: Parser [Ticket]
    nearbyTicketsP = do
      void $ string "nearby tickets:"
      void newline
      manyTill ticketP eof

    ticketP :: Parser Ticket
    ticketP = do
      numbers <- numbersP
      void newline
      pure $ fromList numbers

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
-- count xs v = fromMaybe 0 (c IMap.!? v)
--   where
--     c = IMap.fromListWith (+) . fmap (,1) . toList $ xs

readInt :: Text -> Int
readInt = fromJust . readMaybe . toString

head' :: [c] -> c
head' = fromJust . viaNonEmpty head

-- headMay :: [b] -> Maybe b
-- headMay = viaNonEmpty head

-- tail' :: [a] -> [a]
