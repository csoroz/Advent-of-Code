{-# LANGUAGE TupleSections #-}
import Data.Array.Unboxed
import Data.List.Split

type Point = (Int,Int)
type Line = (Point,Point)
type Check = Point -> Bool
type Grid = UArray Point Int

draw :: Check -> Grid -> Line -> Grid
draw check a = accum (+) a . map (,1) . line check

line :: Check -> Line -> [Point]
line check l@(p,q) | check t = take (n+1) $ iterate (.+.d) p
  where t = both abs u
        d = both signum u
        n = uncurry max t
        u = q .-. p
line _ _ = []

(x,y) .+. (a,b) = (x+a, y+b)
(x,y) .-. (a,b) = (x-a, y-b)

checkHV (x,y) = x==0 || y==0
checkD  (x,y) = x==y
checkHVD t = checkHV t || checkD t

top f = maximum . concat . map (unp . both f)

count :: Grid -> Int
count = length . filter (>1) . elems

solve :: Check -> [Line] -> Int
solve check ls = count $ foldl (draw check) a ls
  where
    a = listArray ((0,0),xy) (repeat 0) :: Grid
    xy = fork (top fst, top snd) ls

parse :: String -> Line
parse = pair . map (pair . map g . splitOn ",") . splitOn " -> "
  where g x = read x :: Int

pair :: [a] -> (a,a)
pair [a,b] = (a,b)

unp :: (a,a) -> [a]
unp (a,b) = [a,b]

both f (a,b) = (f a, f b)
fork (f,g) x = (f x, g x)

main = interact $ show . fork (solve checkHV, solve checkHVD) . map parse . lines
