import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map,(!))
import qualified Data.Map as Map

data Move = Move Int Int Int deriving Show
type Mover = Int -> (String,String) -> (String,String)

simulate :: Mover -> ([String],[Move]) -> [String]
simulate move (xs,ms) = snd $ unzip $ Map.toList 
  $ foldl crane (Map.fromList $ zip [1..] xs) ms
  where crane m (Move n a b)
          = Map.insert a from
          $ Map.insert b to m
          where (from,to) = move n (m!a,m!b)

move9000,move9001 :: Mover
move9000 0 xsys = xsys
move9000 n (x:xs,ys) = move9000 (n-1) (xs,x:ys)
move9001 n (xs,ys) = (zs,ws++ys)
    where (ws,zs) = splitAt n xs

parse :: [String] -> ([String],[Move])
parse xs = (crates ys, map getMove zs)
  where (ys:zs:_) = splitOn [""] xs
        getMove z = Move (read n) (read from) (read to)
                    where (_:n:_:from:_:to:_) = words z
        crates = filter p . map dropSpaces . transpose
          where dropSpaces = dropWhile (==' ')
                p [] = False
                p (x:_) = not $ elem x "[]"

fork (f,g) x = (f x, g x)

main = interact $ show . fork (f,g) . parse . lines
  where f = h . simulate move9000
        g = h . simulate move9001
        h = head . transpose
