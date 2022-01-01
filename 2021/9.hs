import Data.Array.Unboxed
import Data.Maybe
import Data.Char
import Data.List

type Point = (Int,Int)
type Grid = UArray Point Int

lows :: Grid -> [Point]
lows a = catMaybes $ map low (range b)
  where
    b = bounds a
    low p = if all (>x) xs then Just p else Nothing
          where (x:xs) = map (a!) (p:adjacents a p)

adjacents :: Grid -> Point -> [Point]
adjacents a (x,y) = filter (inRange b) $ map go [(0,-1),(-1,0),(1,0),(0,1)]
  where b = bounds a; go (dx,dy) = (x+dx,y+dy)

risk a = sum $ map ((1+).(a!)) (lows a)

basin :: Grid -> Point -> [Point]
basin a x = go [x] [x]
  where
    go [] ys = ys
    go xs ys = go zs (union ys zs)
      where zs = foldr union [] (map g xs)
            g x = filter (p.(a!)) (adjacents a x)
                  where u = a!x; p v = v>u && v<9

score a = product $ take 3 $ reverse $ sort $ map (length . basin a) (lows a)

parse :: [String] -> Grid
parse ls = listArray ((1,1),nm) (map digitToInt $ concat ls)
  where nm = (length ls, length $ head ls)

fork (f,g) x = (f x, g x)

main = interact $ show . fork (risk,score) . parse . lines
