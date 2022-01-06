import Data.List
import Data.List.Extra (groupSort)
import Data.List.Split (splitOn)
import Data.Char (isLower)
import Data.Maybe (fromJust)
import Data.Tuple (swap)

type Node = String
type Path = [Node]
type Edge = (Node,Node)
type Graph = [(Node,[Node])]

small :: Node -> Bool
small = isLower . head

nodes :: Graph -> [Node]
nodes = map fst

succs :: Node -> Graph -> [Node]
succs a = fromJust . lookup a

graph :: [Edge] -> Graph
graph es = map (fmap sort) $ groupSort $ map swap es ++ es

prune :: Node -> Graph -> Graph
prune a = map (fmap (delete a)) . deleteNode a

deleteNode :: Node -> Graph -> Graph
deleteNode a = deleteBy f (a,[])
  where f x y = fst x == fst y

paths :: Node -> Node -> Graph -> [Path]
paths start end g = go start g
  where
    go a g
      | a == end = [[a]]
      | otherwise = [a:p | t <- succs a g, p <- go t g']
      where g' = if small a then prune a g else g

count = length . paths "start" "end"

parse = graph . map (pair . splitOn "-")
  where pair [a,b] = (a,b)

main = interact $ show . count . parse . lines
