import Data.List.Split
import Data.List
import Data.Maybe
import Data.Ord

type Pattern = ([String],[String])

count1478 :: [Pattern] -> Int
count1478 = sum . map (count.length) . concat . map snd
  where
    count 2 = 1 -- 1
    count 3 = 1 -- 7
    count 4 = 1 -- 4
    count 7 = 1 -- 8
    count _ = 0

decode :: Pattern -> Int
decode (xs,ys) = read $ map (fromJust . flip lookup zs) ys
  where
    zs = zip [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] "0123456789"
    [x1,x7,x4,u,v,w,x,y,z,x8] = sortBy (comparing length) xs
    x2 = g 3 x4 x235; x6 = g 4 x7 x690
    x3 = g 2 x7 x235; x9 = g 2 x4 x690
    x5 = head $ x235 \\ [x2,x3]; x235 = [u,v,w]
    x0 = head $ x690 \\ [x6,x9]; x690 = [x,y,z]
    g n x = fst . head . dropWhile p . map f
      where p = (/=n) . length . snd
            f = fmap (\\x) . dup

solve = sum . map (decode . both (map sort))

dup a = (a,a)
both f (a,b) = (f a, f b)
fork (f,g) x = (f x, g x)

pair [a,b] = (a,b)

parse = pair . splitOn ["|"] . words

main = interact $ show . fork (count1478,solve) . map parse . lines
