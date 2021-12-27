import Data.Char
import Data.List

binary :: [Int] -> Integer
binary = foldl (\a b -> 2*a + fromIntegral b) 0

digit :: Char -> Int
digit x = ord x - ord '0'

digits :: String -> [Int]
digits = map digit

neg :: Int -> Int
neg x = 1 - x

most :: String -> (Int,Int)
most = foldl count (0,0)
  where
    count (a,b) '0' = (a+1,b)
    count (a,b) '1' = (a,b+1)

select cmp (a,b) = if cmp a b then '0' else '1'

power :: [String] -> Integer
power = g  . map f . transpose
  where g xs = gamma xs * epsilon xs
        f = digit . select (>) . most
        epsilon = binary . map neg
        gamma   = binary

support :: [String] -> Integer
support xs = generator * scrubber
  where
    generator = rating (>)
    scrubber = rating (<=)
    rating cmp = go 0 xs
      where
        go _ [x] = binary (digits x)
        go n xs  = go (n+1) (f xs)
          where f = filter ((==y).(!!n))
                t = transpose xs !! n
                y = select cmp (most t)

fork (f,g) x = (f x, g x)

main = interact $ show . fork (power,support) . lines
