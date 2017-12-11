module Five where

import Control.Lens
import Data.Sequence
import Lib (stringToIntList)

nextWorld :: Seq Int -> Int -> Seq Int
nextWorld world pos = update pos (+ 1) world

jump :: Seq Int -> Int -> Int -> Int
jump world pos count =
  case world ^? element pos of
    Nothing -> count
    Just nextPos -> jump (nextWorld pos) nextPos (count + 1)

calculateSteps :: Seq Int -> Int
calculateSteps world = jump world 0 0

main :: IO ()
main = do putStrLn "What is your world?"
          world <- readLn
          let world = fromList $ stringToIntList world
          in putStrLn $ show (calculateSteps world)
