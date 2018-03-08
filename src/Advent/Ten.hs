module Advent.Ten where

reverseSelection :: [a] -> Int -> Int -> [a]
reverseSelection list pos len =
  let selection = reverse $ take len $ drop pos $ cycle list
      diff      = length list - len
      remainder = take diff $ drop len $ drop pos $ cycle list
      leftLen   = len - pos + diff
  in case splitAt leftLen selection of
    (a, []) -> a ++ remainder
    ([], b) -> remainder ++ b
    (a, b) -> a ++ remainder ++ b
    -- (a, b) -> b  ++ remainder ++ a

-- hash :: [Int] -> [Int] -> Maybe Int
-- hash inputs numbers =
--  hash' inputs numbers 0 0
--  where hash' :: [Int] -> [Int] -> Int -> Int -> Maybe Int
--        hash' [] (n1:n2:_) _ _ = Just $ n1 * n2
--        hash' [] _ _ _ = Nothing
--        hash' (l:is) nums len skip =
--          let nextNumbers = reverseSelection nums (len + skip) l
--          in hash' is nextNumbers l (skip + 1)
