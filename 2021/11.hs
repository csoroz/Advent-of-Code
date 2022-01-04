import Data.Array
import Data.Char

type Point = (Int,Int)
type Grid a = Array Point a

mapGrid :: (a -> b) -> Grid a -> Grid b
mapGrid f a = listArray (bounds a) $ fmap f $ elems a

mapPoints :: (a -> a) -> [Point] -> Grid a -> Grid a
mapPoints f ps a = a // map (fmap f . g) ps
  where g p = (p,a!p) 

adjacents :: Grid Int -> Point -> [Point]
adjacents a (x,y) = filter (inRange b) (map d ds)
  where b = bounds a
        d (dx,dy) = (x+dx,y+dy); as = [-1,0,1]
        ds = [(x,y) | x <- as, y <- as, (x,y) /= (0,0)]

flashing :: Grid Int -> [Point] -> [Point]
flashing a = filter ((9==).(a!))

flashes :: Grid Int -> [Point]
flashes a = flashing a $ range $ bounds a

flash :: [Point] -> Grid Int -> Grid Int
flash = mapPoints (+1)

gain :: Grid Int -> Grid Int
gain = mapGrid (+1)

nextGrid :: Grid Int -> Grid Int
nextGrid = mapGrid next
next x = if x > 9 then 0 else x

simulate :: Grid Int -> [(Grid Int, [Point])]
simulate a = iterate step (a,[])
  where
    step (a,_) = fmap concat $ go (gain a) [flashes a]
      where
        go a ([]:xss) = (nextGrid a, xss)
        go a (xs:xss) = go a' (zs:xs:xss)
          where
            (a',zs) = foldr f (a,[]) xs
            f x (a,ys) = (flash vs a, zs++ys)
              where vs = adjacents a x
                    zs = flashing a vs

count :: Int -> Grid Int -> Int
count n = length . concat . map snd . take n . tail . simulate

sync :: Grid Int -> Int
sync a = fst $ head $ dropWhile p $ zip [0..] (simulate a)
  where p = (zs/=) . map (const ()) . snd . snd
        zs = map (const ()) $ elems a

parse :: [String] -> Grid Int
parse ls = listArray ((1,1),nm) (map digitToInt $ concat ls)
  where nm = (length ls, length $ head ls)

fork (f,g) x = (f x, g x)

main = interact $ show . fork (count 100, sync) . parse . lines
