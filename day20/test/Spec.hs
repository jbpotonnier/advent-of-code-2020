module Main (main) where

import Aoc
import qualified Data.Map as Map
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

      -- forM_ tiles $ \t -> do
      --   print $ image t
      --   print $ rotate . image $ t
      --   putStrLn "--"

      -- traverse_ (displayTransform transform) tiles

      (and . fmap (\Tile {image} -> image == (transform . transform . transform . transform) image)) tiles `shouldBe` True

    it "search" $ do
      tiles <- readInput "./test/example.txt"
      let initial = allPossibilities 3 tiles
          solutions = search initial

          cs = corners . fmap tileId <$> solutions

      product <$> cs `shouldBe` [20899048083289,20899048083289,20899048083289,20899048083289,20899048083289,20899048083289,20899048083289,20899048083289,20899048083289]

    it "star 1" $ do
      tiles <- readInput "./test/input.txt"
      let initial = allPossibilities 12 tiles
          solutions = search initial

          cs = corners . fmap tileId <$> solutions

      product <$> cs `shouldBe` []

    it "debug" $ do
      tiles <- readInput "./test/example.txt"
      let [t11] = filter ((== 1427) . tileId) tiles
          t11' = Tile 1427 (flipY $ image t11)

          [t01] = filter ((== 2729) . tileId) tiles
          t01' = Tile 2729 (flipY $ image t01)

          [t12] = filter ((== 1489) . tileId) tiles
          t12' = Tile 1489 (flipY $ image t12)

          [t10] = filter ((== 2311) . tileId) tiles
          t10' = Tile 2311 (flipY $ image t10)

          [t21] = filter ((== 2473) . tileId) tiles
          t21' = Tile 2473 (rotate . flipX $ image t21)

      ((1, 2), t12') `shouldSatisfy` isCompatible ((1, 1), t11')
      ((0, 1), t01') `shouldSatisfy` isCompatible ((1, 1), t11')
      ((1, 0), t10') `shouldSatisfy` isCompatible ((1, 1), t11')
      ((2, 1), t21') `shouldSatisfy` isCompatible ((1, 1), t11')

displayTransform :: MonadIO m => (Image -> Image) -> Tile -> m ()
displayTransform g Tile {image} = do
  putTextLn $ showImage image
  putTextLn $ showImage (g image)
  putTextLn "---\n"

corners :: (Ord a, Ord b1, Num a, Num b1) => Map (a, b1) b2 -> [b2]
corners m = fmap (m Map.!) [(0, 0), (0, 2), (2, 2), (2, 0)]