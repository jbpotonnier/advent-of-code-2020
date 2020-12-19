module Main (main) where

import Aoc
import Test.Hspec
import Text.Megaparsec (MonadParsec (eof), parseMaybe)

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

    it "star 2 : example_2_1" $ do
      (rules, messages) <- readInput "./test/example_2_1.txt"
      let p = mkParser rules 0

      filter (accepts p) messages
        `shouldBe` [ "bbabbbbaabaabba",
                     "ababaaaaaabaaab",
                     "ababaaaaabbbaba"
                   ]

    it "star 2 : example_2_2" $ do
      (rules, messages) <- readInput "./test/example_2_2.txt"
      let p = mkParser2 rules 0

      filter (accepts p) messages
        `shouldBe` [ "bbabbbbaabaabba",
                     "babbbbaabbbbbabbbbbbaabaaabaaa",
                     "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
                     "bbbbbbbaaaabbbbaaabbabaaa",
                     "bbbababbbbaaaaaaaabbababaaababaabab",
                     "ababaaaaaabaaab",
                     "ababaaaaabbbaba",
                     "baabbaaaabbaaaababbaababb",
                     "abbbbabbbbaaaababbbbbbaaaababb",
                     "aaaaabbaabaaaaababaa",
                     "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
                     "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
                   ]

    it "star 2" $ do
      (rules, messages) <- readInput "./test/input2.txt"
      let p = mkParser2 rules 0

      (length . filter (accepts p)) messages `shouldBe` 309

accepts :: Parser String -> String -> Bool
accepts p s = isJust $ parseMaybe (p >> eof) s
