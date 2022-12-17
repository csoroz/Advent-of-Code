import Data.List

marker :: Ord a => Int -> [a] -> Int
marker n = go 0
  where go _ [] = 0
        go k xs
          | length (group $ sort $ take n xs) == n = n+k
          | otherwise = go (k+1) (tail xs)

both f (a,b) = (f a, f b)
fork (f,g) x = (f x, g x)

main = interact $ show . fork (both (map . marker) (4,14)) . lines
