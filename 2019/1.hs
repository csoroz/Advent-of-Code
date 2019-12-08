import Data.List

fuel :: Int -> Int
fuel m = div m 3 - 2

fuel' :: Int -> Int
fuel' = sum . tail . takeWhile (>0) . iterate fuel

solve f = sum . map (f . read)

pair (f,g) x = (f x, g x)

main = interact $ show . pair (solve fuel, solve fuel') . lines
