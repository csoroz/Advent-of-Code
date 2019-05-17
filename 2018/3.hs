{-# LANGUAGE TupleSections #-}
import Data.Array.Unboxed (UArray,(!))
import qualified Data.Array.Unboxed as U
import Data.Tuple.Extra
import Data.List.Split (splitOn,splitOneOf)
import Data.List
import Data.Maybe

type P = (Int,Int)
type R = (P,P)

pair   [a,b] = (a,b)
unpair (a,b) = [a,b]

(a,b) !+! (x,y) = (a+x,b+y)
(a,b) !-! (x,y) = (a-x,b-y)

cross xs ys = [(x,y) | x <- xs, y <- ys]

claim (p@(a,b),w) = cross [a..x] [b..y]
      where (x,y) = (p !+! w) !-! (1,1)

slice :: [R] -> UArray P Int
slice ps = U.accumArray (+) 0 ((0,0),m) (concatMap g ps)
  where f = unzip . uncurry (zipWith (!+!)) . unzip
        m = both maximum (f ps)
        g = map (,1) . claim


solve_1 :: UArray P Int -> Int
solve_1 = length . filter (>1) . U.elems

solve_2 :: UArray P Int -> [(Int,R)] -> Int
solve_2 a = fst . fromJust . find f
  where f = all (==1) . map (a!) . claim . snd

parse :: String -> (Int,R)
parse s = (read i, (g "," a, g "x" b))
  where
    [i,a,b] = filter (not.null) (splitOneOf "#@: " s)
    g x = pair . map read . splitOn x

main = interact (show . solve)
  where solve ls = (solve_1 a, solve_2 a xs)
          where xs = map parse (lines ls)
                a = slice (map snd xs)
