module Six where

import Data.Sequence
import Lib (stringToIntList)

type Banks = Seq Int
type SelectedBank = (Int, Int)

chooseBank :: Banks -> SelectedBank
chooseBank banks = foldrWithIndex findBank banks
  where findBank idx (prevIdx, prevMax) val | prevMax > val = (prevIdx, prevMax)
                                            | otherwise = (idx, val)

distributeBank :: Banks -> SelectedBank -> Banks
distributeBank banks (idx, val) =
  let totalBanks = count banks
      acc = 3
      rem = 1
  in mapWithIndex addToBank banks
  where addToBank currIdx currVal | currIdx == idx = rem
                                  | otherwise = val + acc

calculateCycles :: Banks -> Banks -> Int
calculateCycles prevBanks nextBanks cycles
  | prevBanks == nextBanks = cycles
  | otherwise = let banks = distributeBank nextBanks $ chooseBank nextBanks
                in caclulateCycles nextBank banks (cycles + 1)

main :: IO ()
main = do putStrLn "Input banks"
       banks <- readLn
       let banks = fromList $ stringToIntList banks
       in putStrLn $ caclulateCycles empty banks 0
