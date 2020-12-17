module Main (main) where

import Aoc
import Data.List ((!!))
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Aoc" $ do
    it "parse" $ do
      readSpace ".#.\n..#\n###-n"
        `shouldBe` fromList [(0, 1, 0), (1, 2, 0), (2, 0, 0), (2, 1, 0), (2, 2, 0)]

    it "layer" $ do
      layer 0 (fromList [(0, 1, 0), (1, 2, 0), (2, 0, 0), (2, 1, 0), (2, 2, 0)])
        `shouldBe` fromList [(0, 1), (1, 2), (2, 0), (2, 1), (2, 2)]

    it "show layer" $ do
      showLayer (fromList [(0, 1), (1, 2), (2, 0), (2, 1), (2, 2)])
        `shouldBe` ".#.\n..#\n###\n"

    it "transform" $ do
      let initial = readSpace ".#.\n..#\n###-n"
      printLayer 0 initial

      let transformed1 = transform initial
      printLayer (-1) transformed1
      printLayer 0 transformed1
      printLayer 1 transformed1

      let transformed2 = transform transformed1
      printLayer 2 transformed2
      printLayer (-1) transformed2
      printLayer 0 transformed2
      printLayer 1 transformed2
      printLayer 2 transformed2

      let transformed6 = iterate transform initial !! 6
      countActive transformed6 `shouldBe` 112

    it "star 1" $ do
      let initial =
            readSpace . unlines $
              [ "...#..#.",
                "..##.##.",
                "..#.....",
                "....#...",
                "#.##...#",
                "####..##",
                "...##.#.",
                "#.#.#..."
              ]
      let transformed6 = iterate transform initial !! 6
      countActive transformed6 `shouldBe` 247

printLayer :: MonadIO m => Int -> Set (Int, Int, Int) -> m ()
printLayer z coords = do
  putTextLn $ "z=" <> show z
  putTextLn . showLayer . layer z $ coords