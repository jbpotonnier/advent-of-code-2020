{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Day3
  ( Direction (..),
    Grid (..),
    Square (..),
    Step (..),
    makeGrid,
    path,
    readGrid,
    readLine,
    readSquare,
  )
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parse" $ do
    it "should read line" $ do
      readLine "..##......."
        `shouldBe` [ Open,
                     Open,
                     Tree,
                     Tree,
                     Open,
                     Open,
                     Open,
                     Open,
                     Open,
                     Open,
                     Open
                   ]
  it "should read square" $ do
    readSquare "..##\n#..#\n"
      `shouldBe` [ [Open, Open, Tree, Tree],
                   [Tree, Open, Open, Tree]
                 ]

  describe "build the Grid" $ do
    it "should make infinite grid" $ do
      ( fmap (take 6) . unGrid . makeGrid $
          [ [Open, Open, Tree, Tree],
            [Tree, Open, Open, Tree]
          ]
        )
        `shouldBe` [ [Open, Open, Tree, Tree, Open, Open],
                     [Tree, Open, Open, Tree, Tree, Open]
                   ]

  describe "examples" $ do
    grid <- runIO $ readGrid "./test/example.txt"

    it "first star" $ do
      path grid (Step [DirectionRight 3, DirectionDown 1])
        `shouldBe` [Open, Tree, Open, Tree, Tree, Open, Tree, Tree, Tree, Tree]

    it "second star" $ do
      computeSecondStar grid `shouldBe` 336

  describe "stars" $ do
    grid <- runIO $ readGrid "./test/firstInput.txt"

    it "first star" $ do
      let actualPath = path grid (Step [DirectionRight 3, DirectionDown 1])
      count Tree actualPath `shouldBe` 171

    it "second star" $ do
      computeSecondStar grid `shouldBe` 1206576000

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

computeSecondStar :: Grid -> Int
computeSecondStar g = product . fmap (count Tree . path g) $ secondStarSlopes

secondStarSlopes :: [Step]
secondStarSlopes =
  [ Step [DirectionRight 1, DirectionDown 1],
    Step [DirectionRight 3, DirectionDown 1],
    Step [DirectionRight 5, DirectionDown 1],
    Step [DirectionRight 7, DirectionDown 1],
    Step [DirectionRight 1, DirectionDown 2]
  ]