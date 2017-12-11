module Main where

import Test.Hspec
import Lib (calculateCaptcha)

advent1 = do
  it "example one" $ do
    calculateCaptcha [1122] `shouldBe` (3 :: Int)
  it "example two" $ do
    calculateCaptcha [1111] `shouldBe` (4 :: Int)
  it "example three" $ do
    calculateCaptcha [1234] `shouldBe` (0 :: Int)
  it "example four" $ do
    calculateCaptcha [91212129] `shouldBe` (9 :: Int)

spec :: Spec
spec = do
  describe "Adevent of Code: Day 1" advent1

main :: IO ()
main = hspec spec
