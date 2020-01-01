import Data.List
import Control.Arrow

fuel :: Int -> Int
fuel m = div m 3 - 2

fuel' :: Int -> Int
fuel' = sum . tail . takeWhile (>0) . iterate fuel

solve f = sum . map (f . read)

main = interact $ show . (solve fuel &&& solve fuel') . lines
