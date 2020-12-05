{-# LANGUAGE LambdaCase #-}

module Day5 (module Day5) where

import Data.Set (difference, member)

data BoardingPass = BoardingPass [Dir] [Dir]
  deriving (Show, Eq)

data Dir = Low | High
  deriving (Show, Eq)

mySeat :: [Text] -> Maybe [Integer]
mySeat boardingPasses = mySeat' <$> traverse seatId boardingPasses

mySeat' :: [Integer] -> [Integer]
mySeat' boardingPassSeats = filter isNear candidates
  where
    isNear s = all (`member` seatIdSet) [s + 1, s -1]
    seatIdSet = fromList boardingPassSeats
    candidates = toList $ difference allSeatIds seatIdSet
    allSeatIds = fromList [computeSeatId r c | r <- [0 .. 127], c <- [0 .. 7]]

seatId :: Text -> Maybe Integer
seatId s = do
  BoardingPass row col <- parseBoardingPass s
  pure $ computeSeatId (computeRow row) (computeColumn col)

computeSeatId :: Integer -> Integer -> Integer
computeSeatId r c = r * 8 + c

computeRow :: [Dir] -> Integer
computeRow = fst . dicho (0, 127)

computeColumn :: [Dir] -> Integer
computeColumn = fst . dicho (0, 7)

dicho :: Foldable t => (Integer, Integer) -> t Dir -> (Integer, Integer)
dicho = foldl' $ \(lo, hi) dir ->
  let p = (lo + hi) `div` 2
   in case dir of
        Low -> (lo, p)
        High -> (p + 1, hi)

parseBoardingPass :: Text -> Maybe BoardingPass
parseBoardingPass t = case toString t of
  [a, b, c, d, e, f, g, u, v, w] ->
    BoardingPass
      <$> traverse readRow [a, b, c, d, e, f, g]
      <*> traverse readCol [u, v, w]
  _ -> Nothing
  where
    readRow = \case
      'F' -> Just Low
      'B' -> Just High
      _ -> Nothing

    readCol = \case
      'L' -> Just Low
      'R' -> Just High
      _ -> Nothing