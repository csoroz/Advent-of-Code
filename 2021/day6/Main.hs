import qualified Data.Vector as V
import Data.Vector (Vector,(!))
import Data.List.Split

lantern :: Int -> Vector Integer
lantern n = V.unfoldr g 0
  where
    v = V.unfoldr f 0
    f k | k > n = Nothing
    f k | k+9 > n = Just (1, k+1)
    f k = Just (children (k+9), k+1)
    g 9 = Nothing
    g k = Just (children (k+1), k+1) 
    children = (1+) . sum . map (v!) . takeWhile (<=n) . iterate (+7)

count :: Int -> [Int] -> Integer
count n = sum . map (a!)
  where a = lantern n

solve = fork (count 80, count 256)

fork (f,g) x = (f x, g x)

main = interact $ show . solve . g . lines
  where g = map read . splitOn "," . head
