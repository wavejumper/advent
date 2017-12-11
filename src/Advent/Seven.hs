module Advent.Seven where

data Node = Node { nodeName :: String
                 , nodeWeight :: Int
                 , nodeDependencies :: [String]
                 }

type Graph = [Node]

data NodeAst =  Name | Weight | Dependencies

node :: String -> Int -> [String] -> Node
node name weight dependencies
  = Node { nodeName = name
         , nodeWeight = weight
         , nodeDependencies = dependencies
         }

readChar :: String -> String -> NodeAst -> String -> Int -> [String] -> Graph -> Maybe Graph
readChar [] _ Dependencies _ _ _ graph = Just graph
readChar [] _ _ _ _ _ _ = Nothing

readChar (char:rest) state Dependencies name weight dependencies graph
  | char == ',' = readChar rest "" Dependencies name weight (dependencies ++ [state]) graph
  | char == ' ' = readChar rest state Dependencies name weight dependencies graph
  | char == '\n' = readChar rest "" Name "" 0 [] (graph ++ [(node name weight dependencies)])
  | otherwise = readChar rest (state ++ [char]) Dependencies name weight dependencies graph

readChar (')':'-':'>':rest) state Weight name _ dependencies graph
  = case read state of
      Just weight -> readChar rest "" Dependencies name weight dependencies graph
      Nothing -> Nothing

readChar (char:rest) state Weight name _ _ graph
  = readChar rest (state ++ [char]) Weight name 0 [] graph

readChar (char:rest) state Name _ _ _ graph
  | char == '(' = readChar rest "" Weight state 0 [] graph
  | char == ' ' = readChar rest state Name "" 0 [] graph
  | otherwise = readChar rest (state ++ [char]) Name "" 0 [] graph

stringToGraph :: String -> Maybe Graph
stringToGraph s = readChar s "" Name [] 0 [] []
