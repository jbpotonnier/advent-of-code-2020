module Main (main) where

import Aoc
import qualified Data.Set as Set
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Aoc" $ do
    it "parser" $ do
      (rules, messages) <- readInput "./test/parser_example.txt"

      rules
        `shouldBe` fromList
          [ (0, Simple [4, 1, 5]),
            (1, Double [2, 3] [3, 2]),
            (2, Double [4, 4] [5, 5]),
            (3, Double [4, 5] [5, 4]),
            (4, Leaf 'a'),
            (5, Leaf 'b'),
            (106,Double [91] [20])
          ]

      messages `shouldBe` ["ababbb", "bababa", "abbbab", "aaabbb", "aaaabbb"]

    it "match" $ do
      (rules, _) <- readInput "./test/example.txt"

      let possibleMessages = possibles rules 0

      "ababbb" `shouldSatisfy` (`Set.member` possibleMessages)
      "abbbab" `shouldSatisfy` (`Set.member` possibleMessages)

      "bababa" `shouldNotSatisfy` (`Set.member` possibleMessages)
      "aaabbb" `shouldNotSatisfy` (`Set.member` possibleMessages)

    it "star 1" $ do
      (rules, messages) <- readInput "./test/input.txt"
      let possibleMessages = possibles rules 0

      (length . filter (`Set.member` possibleMessages)) messages
        `shouldBe` 195