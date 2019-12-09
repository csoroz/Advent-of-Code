import Data.List.Split
import Data.List
import Data.Ord

count :: Char -> String -> Int
count x = length . filter (==x)

pair (f,g) x = (f x, g x)

check :: [String] -> Int
check = uncurry (*) . pair (count '1', count '2')
      . fst . minimumBy (comparing snd) . map f
      where f xs = (xs, count '0' xs)

decode :: [String] -> String
decode = map g . transpose
  where g = f . head . dropWhile (=='2')
        f '0' = ' '
        f '1' = '*'

main = do
    image <- chunksOf (25*6) <$> head <$> lines <$> getContents
    print $ check image
    mapM_ putStrLn $ chunksOf 25 $ decode image