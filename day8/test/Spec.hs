module Main (main) where

import qualified Data.HashMap.Strict as HashMap
import Day8
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day8" $ do
    it "should parse" $ do
      program <- readProgram "./test/example.txt"
      program
        `shouldBe` Just
          ( HashMap.fromList
              [ (0, Nop),
                (1, Acc 1),
                (2, Jmp 4),
                (3, Acc 3),
                (4, Jmp (-3)),
                (5, Acc (-99)),
                (6, Acc 1),
                (7, Jmp (-4)),
                (8, Acc 6)
              ]
          )
    it "should eval looping program" $ do
      program <- readProgram "./test/example.txt"
      let evaluated = evalProg <$> program <*> pure initEnv
      take 20 . fmap currentInstr <$> evaluated
        `shouldBe` Just [1, 2, 6, 7, 3, 4, 1, 2, 6, 7, 3, 4, 1, 2, 6, 7, 3, 4, 1, 2]

      findLoop <$> evaluated
        `shouldBe` Just (Looping (Env {currentInstr = 1, global = 5}))

    it "first star" $ do
      program <- readProgram "./test/input.txt"
      let evaluated = evalProg <$> program <*> pure initEnv
      findLoop <$> evaluated
        `shouldBe` Just (Looping (Env {currentInstr = 374, global = 2014}))

    it "should eval non looping program" $ do
      let program =
            HashMap.fromList
              [ (0, Nop),
                (1, Acc 1),
                (2, Jmp 4),
                (3, Acc 3),
                (4, Jmp (-3)),
                (5, Acc (-99)),
                (6, Acc 1),
                (7, Nop),
                (8, Acc 6)
              ]
      let evaluated = evalProg program initEnv
      currentInstr <$> evaluated `shouldBe` [1, 2, 6, 7, 8, 9]
      findLoop evaluated `shouldBe` NotLooping (Env {currentInstr = 9, global = 8})

    it "fixProgram" $ do
      let program =
            HashMap.fromList
              [ (0, Nop),
                (1, Acc 1),
                (2, Jmp 4),
                (3, Acc 3),
                (4, Jmp (-3)),
                (5, Acc (-99)),
                (6, Acc 1),
                (7, Jmp (-4)),
                (8, Acc 6)
              ]

      fixProgram program
        `shouldBe` [NotLooping (Env {currentInstr = 9, global = 8})]

    it "second star" $ do
      program <- readProgram "./test/input.txt"
      fixProgram <$> program
        `shouldBe` Just [NotLooping (Env {currentInstr = 638, global = 2251})]