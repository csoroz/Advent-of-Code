main = interact $ show . execute . map parse . lines
  where execute = foldl g 0 where g t (o,x) = o t x
        parse (x:xs) = (op x, read xs)
        op '-' = (-)
        op '+' = (+)
