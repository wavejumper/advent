module Advent.Five where

import Control.Lens
import Data.Sequence
import Lib (stringToIntList)

nextWorld :: Seq Int -> Int -> Seq Int
nextWorld world pos = adjust (+ 1) pos world

jump :: Seq Int -> Int -> Int -> Int
jump world pos count =
  case world ^? element pos of
    Nothing -> count
    Just nextPos -> jump (nextWorld world pos) nextPos (count + 1)

calculateSteps :: Seq Int -> Int
calculateSteps world = jump world 0 0

advent5 :: IO ()
advent5 = do putStrLn "What is your world?"
             str <- readLn
             let world = fromList $ stringToIntList str
                 in putStrLn $ show (calculateSteps world)
