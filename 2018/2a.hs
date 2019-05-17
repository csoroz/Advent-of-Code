import Data.List

main = interact $ show . checksum . map count . lines
    where count = map length . group . sort
          checksum = uncurry (*) . foldr (!+!) (0,0) . map f
            where f xs = (h 2 xs, h 3 xs)
                  h x = fromEnum . any (==x)
                  (a,b) !+! (x,y) = (a+x,b+y)
