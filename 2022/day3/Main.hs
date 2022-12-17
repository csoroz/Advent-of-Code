import Data.Ord
import Data.Char
import Data.List
import Data.List.Split (chunksOf)

priority :: Char -> Int
priority x | isLower x = ord x - ord 'a' + 1
priority x | isUpper x = ord x - ord 'A' + 27

common2 :: (String,String) -> Char
common2 (x:xs,y:ys) = case compare x y of
  EQ -> x
  LT -> common2 (xs,y:ys)
  GT -> common2 (x:xs,ys)

common3 :: (String,String,String) -> Char
common3 (x:xs,y:ys,z:zs)
  | x == y && y == z = x
  | x == m = common3 (xs,y:ys,z:zs)
  | y == m = common3 (x:xs,ys,z:zs)
  | z == m = common3 (x:xs,y:ys,zs)
  where m = minimum [x,y,z]

rucksack :: String -> (String,String)
rucksack s = both sort $ splitAt (div n 2) s where n = length s

triplets = map tuple3 . chunksOf 3 
tuple3 [a,b,c] = (a,b,c)
both f (a,b) = (f a, f b)
fork (f,g) x = (f x, g x)

main = interact $ show . both sum . fork (f,g) . lines
  where f = map (priority . common2 . rucksack)
        g = map (priority . common3) . triplets . map sort
