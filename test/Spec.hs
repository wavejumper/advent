module Main where

import Test.Hspec
import Data.Sequence as Sequence
import Advent.One (calculateCaptcha)
import Advent.Two (calculateChecksum)
import Advent.Four (validPassphrases)
import Advent.Five (calculateSteps)
import Advent.Six (calculateCycles)
import Advent.Seven hiding (advent7)

advent1 :: Spec
advent1 = do
  it "example one" $ do
    calculateCaptcha [1122] `shouldBe` (3 :: Int)
  it "example two" $ do
    calculateCaptcha [1111] `shouldBe` (4 :: Int)
  it "example three" $ do
    calculateCaptcha [1234] `shouldBe` (0 :: Int)
  it "example four" $ do
    calculateCaptcha [91212129] `shouldBe` (9 :: Int)

advent2 :: Spec
advent2 = do
  it "example" $ do
    let spreadsheet = [ [5, 1, 9, 5]
                      , [7, 5, 3]
                      , [2, 4, 6, 8]
                      ]
        in calculateChecksum spreadsheet `shouldBe` (18 :: Int)

advent4 :: Spec
advent4 = do
  it "example" $ do
    let passphrases = [ "aa bb cc dd ee"
                      , "aa bb cc dd aa"
                      , "aa bb cc dd aaa"
                      ]
        in validPassphrases passphrases `shouldBe` (2 :: Int)

advent5 :: Spec
advent5 = do
  it "example" $ do
    let offsets = Sequence.fromList [0, 3, 0, 1, -3]
        in calculateSteps offsets `shouldBe` (5 :: Int)

advent6 :: Spec
advent6 = do
  it "example" $ do
    let banks = Sequence.fromList [0, 2, 7, 0]
        in calculateCycles Sequence.empty banks 0 `shouldBe` (5 :: Int)

advent7 :: Spec
advent7 =
  let str = unlines [ "pbga (66)"
                    , "xhth (57)"
                    , "ebii (61)"
                    , "havc (66)"
                    , "ktlj (57)"
                    , "fwft (72) -> ktlj, cntj, xhth"
                    , "qoyq (66)"
                    , "padx (45) -> pbga, havc, qoyq"
                    , "tknk (41) -> ugml, padx, fwft"
                    , "jptl (61)"
                    , "ugml (68) -> gyxo, ebii, jptl"
                    , "gyxo (61)"
                    , "cntj (57)"
                    ]
      graph = stringToGraph str
  in do
    it "parasing" $ do
      graph `shouldBe` (Just [ (Node "pbga" 66 [])
                             , (Node "xhth" 57 [])
                             , (Node "ebii" 61 [])
                             , (Node "havc" 66 [])
                             , (Node "ktlj" 57 [])
                             , (Node "fwft" 57 ["ktlj", "cntj", "xhth"])
                             , (Node "quyq" 66 [])
                             , (Node "padx" 45 ["pbga", "havc", "qoyq"])
                             , (Node "tknk" 41 ["ugml", "padx", "fwft"])
                             , (Node "jptl" 61 [])
                             , (Node "ugml" 68 ["gyxo", "ebii", "jptl"])
                             , (Node "gyxo" 61 [])
                             , (Node "cntj" 57 [])
                             ])

    it "calculate top" $ do
      (calculateTop' graph) `shouldBe` ((Just (Node "thnk" 41 ["ugml", "padx", "fwft"])) :: Maybe Node)
      where calculateTop' graph =
              case graph of
                Nothing -> Nothing
                Just g -> calculateTop g

spec :: Spec
spec = do
  describe "Advent of Code: Day 1" advent1
  describe "Advent of Code: Day 2" advent2
  describe "Advent of Code: Day 4" advent4
  describe "Advent of Code: Day 5" advent5
  describe "Advent of Code: Day 6" advent6
  describe "Advent of Code: Day 7" advent7

main :: IO ()
main = hspec spec
