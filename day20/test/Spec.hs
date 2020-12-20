module Main (main) where

import Aoc
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Aoc" $ do
    it "parse example" $ do
      tiles <- readInput "./test/example.txt"
      -- traverse_ print tiles
      length tiles `shouldBe` 9

    it "parse input" $ do
      tiles <- readInput "./test/input.txt"
      -- traverse_ print tiles
      length tiles `shouldBe` 144

    it "flip x" $ do
      tiles <- readInput "./test/example.txt"
      let transform = flipX
      -- traverse_ (displayTransform transform) tiles
      (and . fmap (\Tile {image} -> image == (transform . transform) image)) tiles `shouldBe` True

    it "flip y" $ do
      tiles <- readInput "./test/example.txt"
      let transform = flipY
      -- traverse_ (displayTransform transform) tiles
      (and . fmap (\Tile {image} -> image == (transform . transform) image)) tiles `shouldBe` True

    it "rotate" $ do
      tiles <- readInput "./test/example.txt"
      let transform = rotate
      -- traverse_ (displayTransform transform) tiles
      (and . fmap (\Tile {image} -> image == (transform . transform . transform . transform) image)) tiles `shouldBe` True

    fit "search" $ do
      tiles <- readInput "./test/example.txt"
      let initial = allPossibilities 3 tiles
      print $ search initial
      pending

displayTransform :: MonadIO m => (Image -> Image) -> Tile -> m ()
displayTransform g Tile {image} = do
  putTextLn $ showImage image
  putTextLn $ showImage (g image)
  putTextLn "---\n"