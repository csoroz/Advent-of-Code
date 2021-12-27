
(a,b) .+. (x,y) = (a+x, b+y)

solve = p . foldl (.+.) (0,0)
  where p (a,b) = a*b

parse :: [String] -> (Integer, Integer)
parse ["forward", x] = (read x, 0)
parse ["down"   , x] = (0, read x)
parse ["up"     , x] = (0, negate $ read x)

main = interact $ show . solve . map p . lines
  where p = parse . words
