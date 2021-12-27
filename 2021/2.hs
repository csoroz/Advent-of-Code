data Cmd a = F a | D a | U a deriving Show

course (x,y) = x*y

advance = course . foldl (#>) (0,0)
  where
    (x,y) #> F v = (x+v,y)
    (x,y) #> D v = (x,y+v)
    (x,y) #> U v = (x,y-v)

aiming = course . snd . foldl (#>) (0,(0,0))
  where
    (a,(x,y)) #> F t = (a,(x+t,y+a*t))
    (a,(x,y)) #> D t = (a+t,(x,y))
    (a,(x,y)) #> U t = (a-t,(x,y))

parse :: [String] -> Cmd Integer
parse ["forward", x] = F (read x)
parse ["down"   , x] = D (read x)
parse ["up"     , x] = U (read x)

fork (f,g) x = (f x, g x)

main = interact $ show . fork (advance,aiming) . map p . lines
  where p = parse . words
