module Main (main) where

import Day7
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parse rules" $ do
    it "should parse rule" $ do
      parseRule "light red bags contain 1 bright white bag, 2 muted yellow bags."
        `shouldBe` Rule "light red" [(1, "bright white"), (2, "muted yellow")]

      parseRule "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
        `shouldBe` Rule "dark orange" [(3, "bright white"), (4, "muted yellow")]

      parseRule "bright white bags contain 1 shiny gold bag."
        `shouldBe` Rule "bright white" [(1, "shiny gold")]

      parseRule "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
        `shouldBe` Rule "muted yellow" [(2, "shiny gold"), (9, "faded blue")]

      parseRule "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
        `shouldBe` Rule "shiny gold" [(1, "dark olive"), (2, "vibrant plum")]

      parseRule "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
        `shouldBe` Rule "dark olive" [(3, "faded blue"), (4, "dotted black")]

      parseRule "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
        `shouldBe` Rule "vibrant plum" [(5, "faded blue"), (6, "dotted black")]

      parseRule "faded blue bags contain no other bags."
        `shouldBe` Rule "faded blue" []

      parseRule "dotted black bags contain no other bags."
        `shouldBe` Rule "dotted black" []

  describe "First star" $ do
    content <- runIO $ readFileText "./test/example.txt"
    let rules = fmap parseRule . lines $ content

    it "holdersOf" $ do
      sort (holdersOf rules "shiny gold") `shouldBe` ["bright white", "muted yellow"]

    it "should compute first star example" $ do
      let result = listOptions rules "shiny gold"
      result
        `shouldBe` [ ["muted yellow", "shiny gold"],
                     ["bright white", "shiny gold"],
                     ["dark orange", "muted yellow", "shiny gold"],
                     ["light red", "muted yellow", "shiny gold"],
                     ["dark orange", "bright white", "shiny gold"],
                     ["light red", "bright white", "shiny gold"]
                   ]

      countColors result `shouldBe` 4

    it "should compute first star" $ do
      input <- readFileText "./test/input.txt"
      let firstStarRules = fmap parseRule . lines $ input
      length firstStarRules `shouldBe` 594
      let result = listOptions firstStarRules "shiny gold"
      countColors result `shouldBe` 131

  describe "Second star" $ do
    content <- runIO $ readFileText "./test/example2.txt"
    let rules = fmap parseRule . lines $ content

    it "should compute second star example" $ do
      length rules `shouldBe` 7
      countBags rules "shiny gold" `shouldBe` 126

    it "should compute second star" $ do
      input <- readFileText "./test/input.txt"
      let secondStarRules = fmap parseRule . lines $ input
      length secondStarRules `shouldBe` 594
      countBags secondStarRules "shiny gold" `shouldBe` 11261