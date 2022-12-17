{-# LANGUAGE LambdaCase #-}
import Data.Bifunctor

data Hand = Rock | Paper | Scissors deriving Enum

rotate :: Int -> Hand -> Hand
rotate i x = toEnum $ mod (fromEnum x + i) 3

diff :: Hand -> Hand -> Int
diff a b = mod (fromEnum a - fromEnum b) 3

points :: Hand -> Hand -> Int
points a b = case diff a b of
              0 -> 3
              1 -> 0
              2 -> 6

hand :: Hand -> Int
hand Rock     = 1
hand Paper    = 2
hand Scissors = 3

score :: ((Hand,String) -> Hand) -> (Hand,String) -> Int
score h (a,x) = hand b + points a b
  where b = h (a,x)

readABC = \case
  "A" -> Rock
  "B" -> Paper
  "C" -> Scissors

readXYZ = \case
  "X" -> Rock
  "Y" -> Paper
  "Z" -> Scissors

guess :: (Hand,String) -> Hand
guess (_,x) = readXYZ x

decrypt :: (Hand,String) -> Hand
decrypt (a,x) = case x of
  "X" -> rotate 2 a
  "Y" -> a
  "Z" -> rotate 1 a

parse :: String -> (Hand,String)
parse = bimap readABC id . pair . words

pair [a,b] = (a,b)
fork (f,g) x = (f x, g x)

main = interact $ show . fork (f guess, f decrypt) . lines
                  where f h = sum . map (score h . parse)

