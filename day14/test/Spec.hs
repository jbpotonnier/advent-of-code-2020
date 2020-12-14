{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Aoc
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Aoc" $ do
    it "parse example" $ do
      instrs <- readInput "./test/example.txt"
      instrs
        `shouldBe` [ Mask [MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, MX, M1, MX, MX, MX, MX, M0, MX],
                     Mem 8 11,
                     Mem 7 101,
                     Mem 8 0
                   ]

    it "parse input" $ do
      instrs <- readInput "./test/input.txt"
      length instrs `shouldBe` 574

    it "convert to binary" $ do
      toBinary 11
        `shouldBe` [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, True, False, True, True]

      toBinary 73
        `shouldBe` [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, True, False, False, True, False, False, True]

    it "convert from binary" $ do
      fromBinary [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, True, False, True, True]
        `shouldBe` 11

      fromBinary [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, True, False, False, True, False, False, True]
        `shouldBe` 73

    it "star 1" $ do
      instrs <- readInput "./test/input.txt"
      let ComputeState {memory} = executeProgram instrs
      sum memory `shouldBe` 9879607673316

    it "star 2" $ do
      instrs <- readInput "./test/input.txt"
      let ComputeState {memory} = executeProgram2 instrs
      sum memory `shouldBe` 9879607673316