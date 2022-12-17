import Data.List
import Data.List.Split (splitOn)

top :: Ord a => Int -> [a] -> [a]
top n = take n . reverse . sort

parse :: [String] -> [Integer]
parse = map (sum . map read) . splitOn [""]

solve = fork (maximum, sum . top 3)

fork (f,g) x = (f x, g x)

main = interact $ show . solve . parse . lines
