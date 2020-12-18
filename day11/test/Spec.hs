module Main (main) where

import qualified Data.Array.IArray as Array
import Data.Maybe (fromJust)
import Day11
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day11" $ do
    it "should parse array" $ do
      Just a <- readInput "./test/example.txt"
      showLayout a `shouldBe` "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL\n"

    it "should apply rules" $ do
      Just a <- readInput "./test/example.txt"
      let a' = applyRules a
      showLayout a' `shouldBe` "#.##.##.##\n#######.##\n#.#.#..#..\n####.##.##\n#.##.##.##\n#.#####.##\n..#.#.....\n##########\n#.######.#\n#.#####.##\n"

      let c = converge applyRules a
      showLayout c `shouldBe` "#.#L.L#.##\n#LLL#LL.L#\nL.#.L..#..\n#L##.##.L#\n#.#L.LL.LL\n#.#L#L#.##\n..L.L.....\n#L#L##L#L#\n#.LLLLLL.L\n#.#L#L#.##\n"

    it "first star" $ do
      Just a <- readInput "./test/input.txt"
      let c = ((`count` Occupied) . converge applyRules) a
      c `shouldBe` 2424

    it "sees 8" $ do
      let a =
            readLayoutFromList
              [ ".......#.",
                "...#.....",
                ".#.......",
                ".........",
                "..#L....#",
                "....#....",
                ".........",
                "#........",
                "...#....."
              ]
      a Array.! (4, 3) `shouldBe` Empty

      sees a (4, 3) `count` Occupied `shouldBe` 8

    it "sees 0" $ do
      let b =
            readLayoutFromList
              [ ".............",
                ".L.L.#.#.#.#.",
                "............."
              ]
      b Array.! (1, 1) `shouldBe` Empty
      sees b (1, 1) `count` Occupied `shouldBe` 0

  it "first star" $ do
    Just a <- readInput "./test/input.txt"
    let c = ((`count` Occupied) . converge applyRules2) a
    c `shouldBe` 2208

readLayoutFromList :: [Text] -> Layout
readLayoutFromList = fromJust . readLayout . unlines
