module Main (main) where

import Aoc
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Aoc" $ do
    it "parse" $ do
      Right notes <- readInput "./test/better_example.txt"
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
            ticket = fromList [7, 1, 14],
            nearbyTickets =
              [ fromList [7, 3, 47],
                fromList [40, 4, 50],
                fromList [55, 2, 20],
                fromList [38, 6, 12]
              ]
          }

    it "first star" $ do
      Right notes <- readInput "./test/example.txt"
      findInvalidInNotes notes `shouldBe` [4, 55, 12]

      Right starNotes <- readInput "./test/input.txt"
      sum (findInvalidInNotes starNotes) `shouldBe` 20060

    it "valid tickets" $ do
      Right notes <- readInput "./test/example.txt"
      validTicketsInNotes notes
        `shouldBe` [fromList [7, 3, 47]]

    it "field order" $ do
      Right notes <- readInput "./test/field_order_example.txt"
      (fmap name . findFieldOrdersInNotes) notes
        `shouldBe` ["row", "class", "seat"]

    it "field order star 2" $ do
      Right notes <- readInput "./test/input.txt"
      (fmap name . findFieldOrdersInNotes) notes
        `shouldBe` []
