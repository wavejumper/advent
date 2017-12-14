module Advent.Eight where

import Data.HashMap as HashMap hiding (map)
import Text.Read (readMaybe)
import Data.List.Split
import Lib (mapIndexed)

data Fn = Inc | Dec

data Op = If
        | Equals
        | GreaterThan
        | LessThan
        | GreaterThanEquals
        | LessThanEquals
        | NotEquals

data Statement = Symbol String
               | Function Fn
               | Value Int
               | Operator Op

type Register = Map String Int

type Instruction = [Statement]
type Instructions = [Instruction]

readOperator :: String -> Maybe Statement
readOperator "if" = Just $ Operator If
readOperator "==" = Just $ Operator Equals
readOperator "!=" = Just $ Operator NotEquals
readOperator ">"  = Just $ Operator GreaterThan
readOperator ">=" = Just $ Operator GreaterThanEquals
readOperator "<"  = Just $ Operator LessThan
readOperator "<=" = Just $ Operator LessThanEquals
readOperator _ = Nothing

readFn :: String -> Maybe Statement
readFn "inc" = Just $ Function Inc
readFn "dec" = Just $ Function Dec
readFn _ = Nothing

readValue :: String -> Maybe Statement
readValue s =
  case (readMaybe s) :: Maybe Int of
    Just i -> Just $ Value i
    Nothing -> Nothing

readStatement :: Int -> String -> Maybe Statement
readStatement idx s
  | idx == 0 = Just $ Symbol s
  | idx == 1 = readFn s
  | idx == 2 = readValue s
  | idx == 3 = readOperator s
  | idx == 4 = Just $ Symbol s
  | idx == 5 = readOperator s
  | idx == 6 = readValue s
  | otherwise = Nothing

readInstructions :: String -> Maybe Instructions
readInstructions s =
  let readInstruction i = conjStatement [] 0 $ splitOn " " i
      conjStatement instructions idx (x:xs) =
        case readStatement idx x of
          Just stmt -> conjStatement (instructions ++ [stmt]) (idx + 1) xs
          Nothing -> Nothing
      conjStatement instructions _ [] = Just instructions
  in mapM readInstruction $ splitOn "\n" s

updateRegister :: Register -> Fn -> String -> Int -> Register
updateRegister r Inc x v = adjust (+ v) x r
updateRegister r Dec x v = adjust (\y -> y - v) x r

executeInstruction :: Register -> Instruction -> Register
executeInstruction register [(Symbol x), (Function f), (Value xv), (Operator If), (Symbol y), (Operator operator), (Value yv)] =
  let v = case HashMap.lookup y register of
            Just value -> value
            Nothing -> 0
      nextRegister = updateRegister register f x xv
  in case operator of
    Equals            | v == yv -> nextRegister
    NotEquals         | v /= yv -> nextRegister
    GreaterThan       | v > yv  -> nextRegister
    GreaterThanEquals | v >= yv -> nextRegister
    LessThan          | v < yv  -> nextRegister
    LessThanEquals    | v <= yv -> nextRegister
    _ -> register

executeInstruction r _ = r

emptyRegister :: Instructions -> Register
emptyRegister i =
  foldr mkRegister HashMap.empty i
  where
    mkRegister ((Symbol x):_) r = insert x 0 r
    mkRegister _ r = r

executeInstructions :: Instructions -> Register
executeInstructions i = foldl executeInstruction (emptyRegister i) i

largetValue :: Register -> Maybe (String, Int)
largetValue r =
  HashMap.foldWithKey registerMax Nothing r
  where registerMax k v (Just (pk, pv))
          | v > pv = Just (k, v)
          | otherwise = Just (pk, pv)
        registerMax k v Nothing = Just (k, v)

advent8 :: IO ()
advent8 = putStrLn "WIP"
