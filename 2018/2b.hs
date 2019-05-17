main = interact $ same . search . lines
  where same = map fst . filter (uncurry (==)) . uncurry zip
        search xs = head $ [(a,b) | a <- xs, b <- xs, diff a b == 1]
          where diff a b = sum $ map fromEnum $ zipWith (/=) a b