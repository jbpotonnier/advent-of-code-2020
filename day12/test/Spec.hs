module Main (main) where

import Day12
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day12" $ do
    it "Manhattan distance" $ do
      actions <- readInput "./test/example.txt"
      length actions `shouldBe` 5
      let (World position _) = compute (World (0, 0) 90) actions
      manhattan (0, 0) position `shouldBe` 25

    it "star 1" $ do
      actions <- readInput "./test/input.txt"
      length actions `shouldBe` 796
      let (World position _) = compute (World (0, 0) 90) actions
      manhattan (0, 0) position `shouldBe` 1032

    it "Waypoint Manhattan distance" $ do
      actions <- readInput "./test/example.txt"
      let initial = World2 (0, 0) (10, 1)

      let (World2 position _) = compute2 initial actions

      let steps = scanl' next2 initial actions
      traverse_ print steps 

      manhattan (0, 0) position `shouldBe` 286  
    
    it "star 2" $ do
      actions <- readInput "./test/input.txt"
      let initial = World2 (0, 0) (10, 1)

      let (World2 position _) = compute2 initial actions
      manhattan (0, 0) position `shouldBe` 156735