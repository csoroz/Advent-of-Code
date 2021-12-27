data Cmd a = F a | D a | U a deriving Show

(x,y) +# F v = (x+v,y)
(x,y) +# D v = (x,y+v)
(x,y) +# U v = (x,y-v)

solve = p . foldl (+#) (0,0)
  where p (a,b) = a*b

solveAim = p . snd . foldl f (0,(0,0))
  where
    f (a,(x,y)) (F t) = (a,(x+t,y+a*t))
    f (a,(x,y)) (D t) = (a+t,(x,y))
    f (a,(x,y)) (U t) = (a-t,(x,y))
    p (x,y) = x*y

parse :: [String] -> Cmd Integer
parse ["forward", x] = F (read x)
parse ["down"   , x] = D (read x)
parse ["up"     , x] = U (read x)

fork (f,g) x = (f x, g x)

main = interact $ show . fork (solve,solveAim) . map p . lines
  where p = parse . words
