module Advent.Two where

import Data.List.Split
import Lib (stringToIntList)

type Row = [Int]
type Spreadsheet = [Row]

rowDifference :: Row -> Int
rowDifference row = (maximum row) - (minimum row)

calculateChecksum :: Spreadsheet -> Int
calculateChecksum spreadsheet = sum (map rowDifference spreadsheet)

makeSpreadsheet :: String -> Spreadsheet
makeSpreadsheet s = map stringToIntList $ splitOn "\n" s

advent2 :: IO ()
advent2 = do putStrLn "Spreadsheet pls"
             str <- readLn
             let checksum = calculateChecksum (makeSpreadsheet str)
                 in putStrLn $ show checksum
