module Main (main) where

import Aoc
import Test.Hspec
import Text.Megaparsec (parseMaybe)

main :: IO ()
main = hspec $ do
  describe "Aoc" $ do
    it "parser" $ do
      (rules, messages) <- readInput "./test/parser_example.txt"

      rules
        `shouldBe` fromList
          [ (0, Seq [4, 1, 5]),
            (1, Or [2, 3] [3, 2]),
            (2, Or [4, 4] [5, 5]),
            (3, Or [4, 5] [5, 4]),
            (4, One 'a'),
            (5, One 'b'),
            (106, Or [91] [20])
          ]

      messages `shouldBe` ["ababbb", "bababa", "abbbab", "aaabbb", "aaaabbb"]

    it "match" $ do
      (rules, _) <- readInput "./test/example.txt"

      let p = mkParser rules 0

      "ababbb" `shouldSatisfy` accepts p
      "abbbab" `shouldSatisfy` accepts p

      "bababa" `shouldNotSatisfy` accepts p
      "aaabbb" `shouldNotSatisfy` accepts p

    it "star 1" $ do
      (rules, messages) <- readInput "./test/input.txt"
      let p = mkParser rules 0

      (length . filter (accepts p)) messages `shouldBe` 195

accepts :: Parser String -> String -> Bool
accepts p s = isJust $ parseMaybe p s
