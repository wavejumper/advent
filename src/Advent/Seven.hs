module Advent.Seven where

import Text.Read (readMaybe)
import Data.List (find, sort)
import Data.Maybe (mapMaybe)
-- import Data.Ord

data Node = Node { nodeName :: String
                 , nodeWeight :: Int
                 , nodeDependencies :: [String]
                 }
                 deriving (Ord, Eq, Show)
-- {
--instance Ord Node where
--  (Node { nodeWeight = w1 } ) `compare` (Node { nodeWeight = w2 })
--    | w1 <= w2 = LS
--    | otherwise = RS
--

type Graph = [Node]

data NodeAst = NodeAst { parsedState :: String
                       , parsedGraph :: Graph
                       , parsedNode  :: Node
                       }

data NodeAstState = Name NodeAst String
                  | Weight NodeAst String
                  | Dependencies NodeAst String

updateState' :: Char -> NodeAst -> NodeAst
updateState' c n@(NodeAst {parsedState = state}) = n { parsedState = state ++ [c] }

clearState' :: NodeAst -> NodeAst
clearState' n = n { parsedState = "" }

newNode :: Node
newNode = Node "" 0 []

commitNode' :: NodeAst -> NodeAst
commitNode' n@(NodeAst {parsedGraph = graph, parsedNode = currNode}) =
  clearState' n{ parsedGraph = graph ++ [currNode], parsedNode = newNode }

setWeight' :: NodeAst -> Node -> Node
setWeight' NodeAst { parsedState = state } n =
  case readMaybe state :: Maybe Int of
    Just w -> n { nodeWeight = w }
    Nothing -> n

setName' :: NodeAst -> Node -> Node
setName' NodeAst { parsedState = state } n = n { nodeName = state }

conjDependency' :: NodeAst -> Node -> Node
conjDependency' NodeAst { parsedState = state } n@(Node { nodeDependencies = dependencies }) =
  n { nodeDependencies = dependencies ++ [state] }

updateNode' :: NodeAst -> (NodeAst -> Node -> Node) -> NodeAst
updateNode' ast@(NodeAst { parsedNode = n }) f = clearState' ast { parsedNode = f ast n }

readChar :: NodeAstState -> Maybe Graph

readChar (Dependencies ast@(NodeAst { parsedGraph = graph, parsedNode = node, parsedState = state }) [])
  | node == newNode = Just graph
  | state == "" = Just $ graph ++ [node]
  | otherwise = Just $ graph ++ [conjDependency' ast node]

readChar (Weight (NodeAst { parsedGraph = graph, parsedNode = node }) []) = Just $ graph ++ [node]
readChar (Name _ []) = Nothing

readChar (Dependencies ast (char:rest))
  | char == ',' = readChar $ Dependencies (updateNode' ast conjDependency') rest
  | char == ' ' = readChar $ Dependencies ast rest
  | char == '\n' = readChar $ Name (commitNode' ast) rest
  | otherwise = readChar $ Dependencies (updateState' char ast) rest

readChar (Weight ast (')':' ':'-':'>':rest))
  = readChar $ Dependencies (updateNode' ast setWeight') rest

readChar (Weight ast (')':'-':'>':rest))
  = readChar $ Dependencies (updateNode' ast setWeight') rest

readChar (Weight ast (char:rest))
  = readChar $ Weight (updateState' char ast) rest

readChar (Name ast (char:rest))
  | char == '(' = readChar $ Weight (updateNode' ast setName') rest
  | char == ' ' = readChar $ Name ast rest
  | otherwise = readChar $ Name (updateState' char ast) rest

stringToGraph :: String -> Maybe Graph
stringToGraph s = readChar $ Name (NodeAst "" [] newNode) s

data Tree = Tree Node [Tree] | Bottom
  deriving (Ord, Eq, Show)

addToTop :: Node -> Tree -> Node -> Tree

addToTop top Bottom bottom = Tree top [(Tree bottom [])]
addToTop top tree@(Tree currTop deps) bottom
  | bottom == currTop = Tree top [tree]
  | top == currTop = Tree top $ sort (deps ++ [(Tree bottom [])])
  | otherwise = Tree currTop $ sort (map (\d -> addToTop top d bottom) deps)

getDependency :: Graph -> String -> Maybe Node
getDependency graph str =
  find findNode graph
  where findNode (Node {nodeName = name }) = name == str

graphToTree :: Graph -> Tree
graphToTree graph =
  foldl findBottom Bottom graph
  where findBottom tree node@(Node { nodeDependencies = deps } )
          | deps == [] = tree
          | otherwise = foldl (addToTop node) tree $ mapMaybe (getDependency graph) deps

calculateTop :: Graph -> Maybe Node
calculateTop graph =
  case graphToTree graph of
    (Tree top _) -> Just top
    Bottom -> Nothing

advent7 :: IO ()
advent7 = putStrLn "WIP"
