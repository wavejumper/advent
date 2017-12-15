module Advent.Ten where

reverseSelection :: [a] -> Int -> Int -> ([a], [a])
reverseSelection list pos len =
  let selection = reverse $ take len $ drop pos $ cycle list
      remainder = take (length list - length selection) $ drop (pos + len) $ cycle list
  in (selection, remainder)

