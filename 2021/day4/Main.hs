{-# LANGUAGE TupleSections #-}
import Data.List
import Data.List.Split

type Board a = [[a]]
type Mark a = Board (a,Bool)

start :: Board Int -> Mark Int
start = map $ map (,False)

mark :: Int -> Mark Int -> Mark Int
mark x = map $ map check
  where check (y,n) | x==y = (y,True)
        check yn = yn

winner :: Mark Int -> Bool
winner ys = wins zs || wins (transpose zs)
  where zs = map (map snd) ys
        wins = or . map and

score :: Int -> Mark Int -> Int
score x = (x*) . sum . map fst . filter (not.snd) . concat

type Pick a = ([a],[a]) -> Either [a] a
pickFirst, pickLast :: Pick (Mark Int)
pickFirst (z:_,_) = Right z
pickFirst ([],zs) = Left zs
pickLast ([z],[]) = Right z
pickLast ( _ ,zs) = Left zs

play :: Pick (Mark Int) -> ([Int],[Board Int]) -> Int
play pick (xs,ys) = go xs $ map start ys
  where
    go [] _ = 0
    go (x:xs) ys = case pick (partition winner zs) of
        Left zs -> go xs zs
        Right z -> score x z
      where
        zs = map (mark x) ys

parse :: [String] -> ([Int],[Board Int])
parse (a:b:ls) = (f $ splitOn "," a, g $ splitOn [""] ls)
  where
    f ws = map read ws :: [Int]
    g = map $ map $ f . words

fork (f,g) x = (f x, g x)

main = interact $ show . fork (play pickFirst, play pickLast) . parse . lines
