module Four where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split

validWords :: Set String -> [String] -> Bool
validWords _ [] = True
validWords seen (word:words) =
  case Set.member seen word of
    True -> False
    False -> validWords (Set.insert seen word) words

validPassphrase :: String -> Bool
validPassphrase passphrashe =
  validWords Set.empty $ splitOn " " passphrase

validPassphrases :: String -> Int
validPassphrases passphrases = foldr (+) $ filter validPassphrase passphrases

main :: IO ()
main = do putStrLn "What are your passphrases?"
          passphrases <- readLn
          putStrLn $ show (validPassphrases passphrases)
