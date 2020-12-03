module Main (main) where

import MyLib
  ( Policy (..),
    checkPassword,
    checkPassword2,
    readLine,
    readPolicy,
  )
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Parse" $ do
    it "readPolicy" $ do
      readPolicy "1-3 b" `shouldBe` Right (Policy 1 3 'b')
      readPolicy "1-13 b" `shouldBe` Right (Policy 1 13 'b')

    it "readLine" $ do
      readLine "1-3 a: abcde" `shouldBe` Right (Policy 1 3 'a', "abcde")
      
  describe "Check" $ do
    it "checkPassword" $ do
      checkPassword (Policy 1 3 'a') "abcde" `shouldBe` True
      checkPassword (Policy 1 3 'b') "cdefg" `shouldBe` False
      checkPassword (Policy 2 9 'c') "ccccccccc" `shouldBe` True

    it "checkPassword2" $ do
      checkPassword2 (Policy 1 3 'a') "abcde" `shouldBe` True
      checkPassword2 (Policy 1 3 'b') "cdefg" `shouldBe` False
      checkPassword2 (Policy 2 9 'c') "ccccccccc" `shouldBe` False