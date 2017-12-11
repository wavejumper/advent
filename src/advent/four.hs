module Advent.Four where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split

validWords :: Set String -> [String] -> Bool
validWords _ [] = True
validWords seen (word:remWords) =
  case Set.member word seen of
    True -> False
    False -> validWords (Set.insert word seen) remWords

validPassphrase :: String -> Bool
validPassphrase passphrase =
  validWords Set.empty $ splitOn " " passphrase

validPassphrases :: [String] -> Int
validPassphrases passphrases = length $ filter validPassphrase passphrases

main :: IO ()
main = do putStrLn "What are your passphrases?"
          str <- readLn
          let passphrases = splitOn "\n" str
              in putStrLn $ show (validPassphrases passphrases)
