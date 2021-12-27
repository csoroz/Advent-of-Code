data Cmd a = F a | D a | U a deriving Show

solve = p . snd . foldl f (0,(0,0))
  where
    f (a,(x,y)) (F t) = (a,(x+t,y+a*t))
    f (a,(x,y)) (D t) = (a+t,(x,y))
    f (a,(x,y)) (U t) = (a-t,(x,y))
    p (x,y) = x*y

parse :: [String] -> Cmd Integer
parse ["forward", x] = F (read x)
parse ["down"   , x] = D (read x)
parse ["up"     , x] = U (read x)

main = interact $ show . solve . map p . lines
  where p = parse . words
