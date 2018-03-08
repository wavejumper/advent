module Lib where

import Data.List.Split
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed f a =
  m [] 0 a
  where m b i (x:xs) = m (b ++ [(f i x)]) (i + 1) xs
        m b _ [] = b

stringToIntList' :: String -> String -> [Int]
stringToIntList' s delem = mapMaybe readMaybeInt $ splitOn delem s
  where readMaybeInt = readMaybe :: String -> Maybe Int

stringToIntList :: String -> [Int]
stringToIntList s = stringToIntList' s "\t"
