{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Day4 (isValid, parsePassport, parsePassports, validatePassport)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "should parse fields" $ do
      let t = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm"
      parsePassport t
        `shouldBe` fromList
          [ ("ecl", "gry"),
            ("pid", "860033327"),
            ("eyr", "2020"),
            ("hcl", "#fffffd"),
            ("byr", "1937"),
            ("iyr", "2017"),
            ("cid", "147"),
            ("hgt", "183cm")
          ]

    it "should parse example" $ do
      content <- readFileText "./test/example.txt"
      let passports = parsePassports content
      length passports `shouldBe` 4

      passports !!? 3
        `shouldBe` Just
          ( fromList
              [ ("hcl", "#cfa07d"),
                ("eyr", "2025"),
                ("pid", "166559648"),
                ("iyr", "2011"),
                ("ecl", "brn"),
                ("hgt", "59in")
              ]
          )

      fmap isValid passports `shouldBe` [True, False, True, False]
      (length . filter isValid) passports `shouldBe` 2

    it "should count valid passport for first star" $ do
      content <- readFileText "./test/input.txt"
      let passports = parsePassports content
      (length . filter isValid) passports `shouldBe` 260

  describe "Validation" $ do
    it "should be invalid" $ do
      content <- readFileText "./test/invalid.txt"
      let passports = parsePassports content
      length passports `shouldBe` 4
      passports `shouldSatisfy` all (isNothing . validatePassport)

    it "should be valid" $ do
      content <- readFileText "./test/valid.txt"
      let passports = parsePassports content
      length passports `shouldBe` 4
      passports `shouldSatisfy` all (isJust . validatePassport)

    it "should count the valid passports for second star" $ do
      content <- readFileText "./test/input.txt"
      let passports = parsePassports content
      length passports `shouldBe` 291
      (length . filter isJust . fmap validatePassport) passports `shouldBe` 153
