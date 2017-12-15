module Advent.Nine where

data ClosingOver = Group | Garbage

readStream :: String -> [ClosingOver] -> Int -> Int
readStream ('!':_:xs) c i = readStream xs c i
readStream ('{':xs) c i = readStream xs ([Group] ++ c) i
readStream ('<':xs) c i = readStream xs ([Garbage] ++ c) i
readStream ('>':xs) (Garbage:c) i = readStream xs c i
readStream ('}':xs) d@(Group:c) i = readStream xs c (i + length d)
readStream (_:xs) c i = readStream xs c i
readStream [] _ i = i
