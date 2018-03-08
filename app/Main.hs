module Main where

import Advent.One
import Advent.Two
import Advent.Four
import Advent.Five
import Advent.Six
import Advent.Seven
import Advent.Eight
import Advent.Nine
import Advent.Ten
import Advent.Twelve

selectDay :: String -> IO ()
selectDay "1" = advent1
selectDay "2" = advent2
selectDay "4" = advent4
selectDay "5" = advent5
selectDay "6" = advent6
selectDay "7" = advent7
selectDay "8" = advent8
selectDay "9" = advent9
selectDay  _  = putStrLn "Invalid day"

main :: IO ()
main = do putStrLn "Choose a day in the advent calendar"
          day <- readLn
          selectDay day
          main
