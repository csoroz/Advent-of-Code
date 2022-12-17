import Data.List
import Data.List.Split (splitOn)

type Range = (Integer,Integer)

covers,overlaps :: Range -> Range -> Bool
covers   (a,b) (x,y) = a <= x && b >= y
overlaps (a,b) (x,y) = a <= x && x <= b

covered,overlapped :: (Range,Range) -> Bool
covered    = anywise covers
overlapped = anywise overlaps

anywise :: (a -> a -> Bool) -> (a,a) -> Bool
anywise p (r,s) = p r s || p s r

count :: (a -> Bool) -> [a] -> Int
count p = sum . map (fromEnum . p)

parse :: String -> (Range,Range)
parse = p (p read "-") ","
  where p f s = pair . take 2 . map f . splitOn s

pair [a,b] = (a,b)
fork (f,g) x = (f x, g x)

main = interact $ show . fork (f,g) . map parse . lines
  where f = count covered; g = count overlapped
