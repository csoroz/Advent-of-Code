import Data.List.Split
import Data.List

median :: [Int] -> Int
median xs = sort xs !! div n 2
  where n = length xs

dist :: [Int] -> Int
dist xs = sum $ map (abs.(m-)) xs
  where m = median xs

fuel :: Int -> [Int] -> Int
fuel m = sum . map (f.abs.(m-))
  where f n = (n+1)*n `div` 2

mean :: [Int] -> Int
mean xs = sum xs `div` length xs

crabs :: [Int] -> Int
crabs xs = uncurry min $ both (flip fuel xs) (m, m+1)
  where m = mean xs

both f (a,b) = (f a, f b)
fork (f,g) x = (f x, g x)

main = interact $ show . fork (dist,crabs) . g . lines
  where g = map read . splitOn "," . head
