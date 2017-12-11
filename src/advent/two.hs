module Two where

import Data.List.Split
import Lib (stringToIntList)

type Row = [Int]
type Spreadsheet = [Row]

rowDifference :: Row -> Int
rowDifference row = (max row) - (min row)

calculateChecksum :: Spreadsheet -> Int
calculateChecksum spreadsheet = foldr (+) (map rowDifference spreadsheet)

makeSpreadsheet :: String -> Spreadsheet
makeSpreadsheet s = map stringToIntList $ splitOn "\n" s

main :: IO ()
main = do putStrLn "Spreadsheet pls"
          spreadsheet <- readLn
          putstrLn $ calculateChecksum (makeSpreadsheet spreadsheet)
