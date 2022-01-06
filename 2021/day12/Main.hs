import qualified Data.Map.Strict as M
import Data.Map.Strict (Map,(!))
import Data.List
import Data.List.Extra (groupSort)
import Data.List.Split (splitOn)
import Data.Char (isUpper)
import Data.Tuple (swap)

type Node = String
type Path = [Node]
type Edge = (Node,Node)
type Graph = Map Node [Node]

graph :: Node -> Node -> [Edge] -> Graph
graph start end es = M.fromList $ canon $ groupSort $ f (es ++ map swap es)
  where f = filter (\(a,b) -> a /= end && b /= start)
        canon = map (fmap sort) -- optional

paths :: Int -> Node -> Node -> Graph -> [Path]
paths n start end g = go start (n, M.fromList [(t,0) | t <- smalls])
  where smalls = filter (not.big) (M.keys g) \\ [start]
        big = isUpper . head
        go a (k,vs)
          | a == end = [[a]]
          | otherwise = [a:p | t <- g!a
                             , let (check,kvs) = visit t
                             , check, p <- go t kvs]
          where visit t | t == end    = (True , (k,vs))
                        | big t       = (True , (k,vs))
                        | v <  k      = (True , (k,vs'))
                        | v == k      = (True , (1,vs'))
                        | otherwise   = (False, (1,vs'))
                        where (v,vs') = (vs!t + 1, M.insert t v vs)

count n = length . paths n "start" "end"

parse = graph "start" "end" . map (pair . splitOn "-")
  where pair [a,b] = (a,b)

fork (f,g) x = (f x, g x)

main = interact $ show . fork (count 1, count 2) . parse . lines
