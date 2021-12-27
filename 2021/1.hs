import Data.List

count :: Ord a => [a] -> Int
count xs = sum $ map fromEnum $ zipWith (<) xs (tail xs)

slide :: Num a => Int -> [a] -> [a]
slide n = map sum . takeWhile p . transpose . take n . iterate tail
  where p = (n==) . length

fork (f,g) x = (f x, g x)

solve = fork (count, count . slide 3)

main = interact $ show . solve . map f . lines
  where f x = read x :: Integer
