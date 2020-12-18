module Main (main) where

import Aoc
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (ParseErrorBundle, Parsec, parse)

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "add" $ do
      parseText expP "12 + 14" `shouldParse` Add (Number 12) (Number 14)

    it "mult" $ do
      parseText expP "2 * 5" `shouldParse` Mult (Number 2) (Number 5)

    it "parens" $ do
      parseText expP "(1 + 3)" `shouldParse` Add (Number 1) (Number 3)
      parseText expP "(12 * 14)" `shouldParse` Mult (Number 12) (Number 14)
      parseText expP "((8 * 9))" `shouldParse` Mult (Number 8) (Number 9)

    it "assoc" $ do
      parseText expP "1 + 2 * 3"
        `shouldParse` Mult (Add (Number 1) (Number 2)) (Number 3)

      parseText expP "2 * 3 + (4 * 5)"
        `shouldParse` Add (Mult (Number 2) (Number 3)) (Mult (Number 4) (Number 5))

  describe "Aoc" $ do
    it "examples" $ do
      compute "2 * 3 + (4 * 5)" `shouldBe` Just 26
      compute "5 + (8 * 3 + 9 + 3 * 4 * 3)" `shouldBe` Just 437
      compute "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" `shouldBe` Just 12240
      compute "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" `shouldBe` Just 13632

    it "star 1" $ do
      exps <- readInput "./test/input.txt"
      length exps `shouldBe` 374
      (sum . fmap eval) exps `shouldBe` 67800526776934

    it "example 2 " $ do
      compute2 "1 + (2 * 3) + (4 * (5 + 6))" `shouldBe` Just 51
      compute2 "2 * 3 + (4 * 5)" `shouldBe` Just 46
      compute2 "5 + (8 * 3 + 9 + 3 * 4 * 3)" `shouldBe` Just 1445
      compute2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" `shouldBe` Just 669060
      compute2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" `shouldBe` Just 23340

    it "star 2" $ do
      exps <- readInput2 "./test/input.txt"
      length exps `shouldBe` 374
      (sum . fmap eval) exps `shouldBe` 340789638435483

parseText :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parseText p = parse p "inputText"