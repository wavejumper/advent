module Lib where

import Data.List.Split
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed f a =
  m [] 0 a
  where m b i (x:xs) = m (b ++ [(f i x)]) (i + 1) xs
        m b _ [] = b

stringToIntList :: String -> [Int]
stringToIntList s = mapMaybe readMaybeInt $ splitOn "\t" s
  where readMaybeInt = readMaybe :: String -> Maybe Int
