module Main (main) where

import qualified Data.Array.IArray as Array
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
      printLayout a'
      showLayout a' `shouldBe` "#.##.##.##\n#######.##\n#.#.#..#..\n####.##.##\n#.##.##.##\n#.#####.##\n..#.#.....\n##########\n#.######.#\n#.#####.##\n"

      let c = converge applyRules a
      printLayout c
      showLayout c `shouldBe` "#.#L.L#.##\n#LLL#LL.L#\nL.#.L..#..\n#L##.##.L#\n#.#L.LL.LL\n#.#L#L#.##\n..L.L.....\n#L#L##L#L#\n#.LLLLLL.L\n#.#L#L#.##\n"

    it "first star" $ do
      Just a <- readInput "./test/input.txt"
      let c = ((`count` Occupied) . Array.elems . converge applyRules) a
      c `shouldBe` 2424

printLayout :: Layout -> IO ()
printLayout = putTextLn . showLayout