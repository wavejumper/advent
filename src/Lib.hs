module Lib where

import Data.List.Split
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

stringToIntList :: String -> [Int]
stringToIntList s = mapMaybe readMaybeInt $ splitOn "\t" s
  where readMaybeInt = readMaybe :: String -> Maybe Int
