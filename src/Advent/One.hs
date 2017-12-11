module Advent.One where

matchPair :: Int -> [Int] -> Int -> Int
matchPair _ [] count = count
matchPair left (right:rest) count | left == right = matchPair right (tail rest) (count + left)
                                  | otherwise = matchPair right (tail rest) count

calculateCaptcha :: [Int] -> Int
calculateCaptcha [] = 0
calculateCaptcha captcha = matchPair (last captcha) captcha 0

readCaptcha :: String -> [Int] -> Maybe [Int]
readCaptcha [] parsed = Just parsed
readCaptcha (input:xs) parsed =
  case read $ show input of
    Just x -> readCaptcha xs (parsed ++ x)
    Nothing -> Nothing

main :: IO ()
main = do putStrLn "What is the captcha?"
          captcha <- readLn
          case readCaptcha captcha [] of
            Just x -> putStrLn $ show (calculateCaptcha x)
            Nothing -> putStrLn "Failed to read captcha"
