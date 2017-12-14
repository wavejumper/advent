module Advent.Eight where

import Data.HashMap as HashMap

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

-- readStatement :: Instructions -> String -> Instructions
-- readStatement instructions ('i':'n':'c':xs) = Function Inc

updateRegister :: Register -> Fn -> String -> Int -> Register
updateRegister r Inc x v = adjust (+ v) x r
updateRegister r Dec x v = adjust (- v) x r

executeInstruction :: Register -> Instruction -> Register
executeInstruction r _ = r
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

largetValue :: Register -> (String, Int)
largetValue r =
  HashMap.foldWithKey registerMax ("", 0) r
  where registerMax (k, v) (pk, pv)
          | v > pv = (k, v)
          | otherwise = (pk, pv)

emptyRegister :: Instructions -> Register
emptyRegister i =
  foldr mkRegister HashMap.empty i
  where
    mkRegister ((Symbol x):_) r = insert x 0 r
    mkRegister _ r = r

executeInstructions :: Instructions -> Register
executeInstructions i = foldl executeInstruction (emptyRegister i) i
