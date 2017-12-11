module Advent.Six where

import Data.Sequence as Sequence
import Lib (stringToIntList)

type Banks = Seq Int
type SelectedBank = (Int, Int)

chooseBank :: Banks -> SelectedBank
chooseBank banks = foldrWithIndex findBank (0, 0) banks
  where findBank idx val (prevIdx, prevMax) | prevMax > val = (prevIdx, prevMax)
                                            | otherwise = (idx, val)

distributeBank :: Banks -> SelectedBank -> Banks
distributeBank banks (idx, val) =
  let totalBanks = Sequence.length banks
      acc = val `quot ` totalBanks
      remainder = val - (acc * totalBanks)
      addToBank currIdx currVal
        | currIdx == idx = remainder
        | otherwise = currVal + acc
      in mapWithIndex addToBank banks

calculateCycles :: Banks -> Banks -> Int -> Int
calculateCycles prevBanks nextBanks cycles
  | prevBanks == nextBanks = cycles
  | otherwise = let selectedBank = chooseBank nextBanks
                    banks = distributeBank nextBanks selectedBank
                in calculateCycles nextBanks banks (cycles + 1)

main :: IO ()
main = do putStrLn "Input banks"
          str <- readLn
          let banks = fromList $ stringToIntList str
              in putStrLn $ show (calculateCycles Sequence.empty banks 0)
