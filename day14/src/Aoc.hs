{-# LANGUAGE NamedFieldPuns #-}

module Aoc
  ( module Aoc,
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Text.Megaparsec
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data Instr w
  = Mask [MaskValue]
  | Mem w w
  deriving (Show, Eq)

type Binary = [Bool]

data ComputeState w = ComputeState
  { memory :: Map w w,
    currentMask :: [MaskValue]
  }
  deriving (Show, Eq)

data MaskValue = M0 | M1 | MX deriving (Show, Eq)

executeProgram2 :: [Instr Integer] -> ComputeState Integer
executeProgram2 = foldl' executeInstr2 initialComputeState

executeInstr2 :: ComputeState Integer -> Instr Integer -> ComputeState Integer
executeInstr2 computeState@ComputeState {currentMask} = \case
  Mask mask -> computeState {currentMask = mask}
  Mem address value -> insertAtAll (allAddresses address) value computeState
  where
    allAddresses :: Integer -> [Integer]
    allAddresses = fmap fromBinary . applyMask currentMask . toBinary

    applyMask :: [MaskValue] -> [Bool] -> [[Bool]]
    applyMask mvs bs = zipWithM g mvs bs

    g :: MaskValue -> Bool -> [Bool]
    g mv bv = case mv of
      M0 -> [bv]
      M1 -> [True]
      MX -> [False, True]

    insertAtAll :: [Integer] -> Integer -> ComputeState Integer -> ComputeState Integer
    insertAtAll as v s = foldl' (insertAt v) s as

    insertAt :: Integer -> ComputeState Integer -> Integer -> ComputeState Integer
    insertAt v s@ComputeState {memory} a = s {memory = Map.insert a v memory}

executeProgram :: [Instr Integer] -> ComputeState Integer
executeProgram = foldl' executeInstr initialComputeState

executeInstr :: ComputeState Integer -> Instr Integer -> ComputeState Integer
executeInstr s@ComputeState {currentMask, memory} = \case
  Mask mask -> s {currentMask = mask}
  Mem address value -> s {memory = Map.insert address (applyMask currentMask value) memory}
  where
    applyMask :: [MaskValue] -> Integer -> Integer
    applyMask mask = fromBinary . applyMaskToBinary mask . toBinary

    applyMaskToBinary :: [MaskValue] -> [Bool] -> [Bool]
    applyMaskToBinary = zipWith g

    g mv bv = case mv of
      M0 -> False
      M1 -> True
      MX -> bv

initialComputeState :: ComputeState w
initialComputeState = ComputeState Map.empty []

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

    readMask :: String -> [MaskValue]
    readMask = fmap toMask
      where
        toMask = \case
          '1' -> M1
          '0' -> M0
          'X' -> MX
          c -> error $ "toBool:" <> show c <> "is not 0 or 1"

-----------------------------------

readInteger :: Text -> Integer
readInteger = fromJust . readMaybe . toString
