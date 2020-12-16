module Main (main) where

import Aoc
import Test.Hspec
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = hspec $ do
  describe "Aoc" $ do
    it "Parse" $ do
      -- Left e <- readInput "./test/example.txt"
      -- putStrLn $ errorBundlePretty e
      -- pending
      
      Right notes <- readInput "./test/example.txt"
      notes
        `shouldBe` Notes
          { fields =
              [ Field
                  { name = "departure location",
                    ranges = [Range {start = 37, end = 594}, Range {start = 615, end = 952}]
                  },
                Field
                  { name = "class",
                    ranges = [Range {start = 1, end = 3}, Range {start = 5, end = 7}]
                  },
                Field
                  { name = "row",
                    ranges = [Range {start = 6, end = 11}, Range {start = 33, end = 44}]
                  },
                Field
                  { name = "seat",
                    ranges = [Range {start = 13, end = 40}, Range {start = 45, end = 50}]
                  }
              ],
            ticket = Ticket [7, 1, 14],
            nearbyTickets =
              [ Ticket [7, 3, 47],
                Ticket [40, 4, 50],
                Ticket [55, 2, 20],
                Ticket [38, 6, 12]
              ]
          }
