{-# LANGUAGE NamedFieldPuns #-}

module Aoc
  ( module Aoc,
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Megaparsec
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data Instr w
  = Mask [(Int, Bool)]
  | Mem w w
  deriving (Show, Eq)

type Binary = Vector Bool

data ComputeState w = ComputeState
  { memory :: Map w w,
    currentMask :: [(Int, Bool)]
  }
  deriving (Show, Eq)

executeProgram :: [Instr Integer] -> ComputeState Integer
executeProgram = foldl' executeInstr initialComputeState

initialComputeState :: ComputeState w
initialComputeState = ComputeState Map.empty []

executeInstr :: ComputeState Integer -> Instr Integer -> ComputeState Integer
executeInstr s@ComputeState {currentMask, memory} = \case
  Mask mask -> s {currentMask = mask}
  Mem address value -> s {memory = Map.insert address (applyMask currentMask value) memory}

applyMask :: [(Int, Bool)] -> Integer -> Integer
applyMask mask v = fromBinary (toBinary v V.// mask)

toBinary :: Integer -> Binary
toBinary = fromList . pad 36 False . reverse . fmap integerToBool . go
  where
    go n
      | n == 0 = []
      | otherwise = let (q, r) = n `divMod` 2 in r : go q

    pad :: Show a => Int -> a -> [a] -> [a]
    pad n x xs =
      let diff = n - length xs
       in if diff >= 0
            then replicate diff x ++ xs
            else error $ "pad: list is too long" <> show xs

    integerToBool :: Integer -> Bool
    integerToBool = \case
      0 -> False
      1 -> True
      i -> error $ "integerToBool:" <> show i <> "is not 0 or 1"

fromBinary :: Binary -> Integer
fromBinary = foldl' (\s b -> boolToInt b + 2 * s) 0
  where
    boolToInt = bool 0 1

readInput :: FilePath -> IO [Instr Integer]
readInput path = mapMaybe readLine . lines <$> readFileText path
  where
    readLine l = parseMaybe lineParser l

    lineParser = maskParser <|> memParser

    maskParser = do
      void $ string "mask = "
      mask <- readMask <$> P.some (choice [char 'X', char '1', char '0'])
      pure $ Mask mask

    memParser = do
      void $ string "mem"
      addressWord <- between (char '[') (char ']') wordParser
      void $ string " = "
      Mem addressWord <$> wordParser

    wordParser :: Parser Integer
    wordParser = readInteger . toText <$> P.some digitChar

    readMask :: String -> [(Int, Bool)]
    readMask =
      fmap (second toBool)
        . filter (\(_, c) -> c /= 'X')
        . zip [0 ..]
      where
        toBool = \case
          '1' -> True
          '0' -> False
          c -> error $ "toBool:" <> show c <> "is not 0 or 1"

-----------------------------------

-- applyTimes :: (c -> c) -> Int -> c -> c
-- applyTimes f n = (!! n) . iterate f

-- count :: (Ord k, Foldable t) => t k -> k -> Int
-- count xs x = fromMaybe 0 (c IMap.!? x)
--   where
--     c = IMap.fromListWith (+) . fmap (,1) . toList $ xs

readInteger :: Text -> Integer
readInteger = fromJust . readMaybe . toString

-- head' :: [c] -> c
-- head' = fromJust . viaNonEmpty head

-- headMay :: [b] -> Maybe b
-- headMay = viaNonEmpty head

-- tail' :: [a] -> [a]
