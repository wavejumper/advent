module Advent.Twelve where

import qualified Data.HashMap as HashMap
import Data.List.Split
import Lib (stringToIntList')
import Text.Read

type Program = Int
type Programs = HashMap.Map Program [Program]

hasVisited :: [Program] -> Program -> Bool
hasVisited programs program = elem program programs

countPrograms :: [Program] -> Programs -> Program -> Int
countPrograms visited programs program =
  case HashMap.lookup program programs of
    Just ps -> let fps = filter (hasVisited visited) ps
                   f   = countPrograms (visited ++ fps) programs
               in foldl (+) (length fps) $ map f fps
    Nothing -> 0

parsePrograms :: String -> Programs
parsePrograms s =
  foldl parseProgram HashMap.empty $ splitOn "\n" s
  where parseProgram programs x =
          let (p:ps:_) = splitOn "<->" x
              psx      = stringToIntList' ps ", "
          in case readMaybe p :: Maybe Int of
            Just px -> HashMap.insert px psx programs
            Nothing -> programs
