{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Aoc
import qualified Data.Map.Strict as Map
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Aoc" $ do
    it "parse example" $ do
      instrs <- readInput "./test/example.txt"
      instrs
        `shouldBe` [ Mask [(29, True), (34, False)],
                     Mem 8 11,
                     Mem 7 101,
                     Mem 8 0
                   ]

    it "parse input" $ do
      instrs <- readInput "./test/input.txt"
      length instrs `shouldBe` 574

    it "convert to binary" $ do
      toBinary 11
        `shouldBe` fromList [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, True, False, True, True]

      toBinary 73
        `shouldBe` fromList [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, True, False, False, True, False, False, True]

    it "convert from binary" $ do
      (fromBinary . fromList) [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, True, False, True, True]
        `shouldBe` 11

      (fromBinary . fromList) [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, True, False, False, True, False, False, True]
        `shouldBe` 73

    it "execute program" $ do
      instrs <- readInput "./test/example.txt"
      executeProgram instrs
        `shouldBe` ComputeState
          { memory = fromList [(7, 101), (8, 64)],
            currentMask = [(29, True), (34, False)]
          }

    it "star 1" $ do
      instrs <- readInput "./test/input.txt"
      let ComputeState {memory} = executeProgram instrs
      sum memory `shouldBe` 9879607673316