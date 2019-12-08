import Data.Maybe
import Data.List.Split

data Segment = H Int (Int,Int) | V Int (Int,Int) deriving Show
data Twist = L | R | U | D deriving Show
type Turn = (Twist,Int)
type Wire = [Turn] 
type Point = (Int,Int)

segments :: Wire -> [Segment]
segments = snd . foldl turn ((0,0),[])
  where
    turn ((x,y),zs) (L,a) = ((x',y), H y (x',x):zs) where x' = x - a
    turn ((x,y),zs) (R,a) = ((x',y), H y (x,x'):zs) where x' = x + a
    turn ((x,y),zs) (U,a) = ((x,y'), V x (y,y'):zs) where y' = y + a
    turn ((x,y),zs) (D,a) = ((x,y'), V x (y',y):zs) where y' = y - a

path :: Point -> Wire -> Wire
path (x0,y0) = reverse . snd . turn ((0,0),[])
  where
    turn (p,zs) (t:ts) = case cmp p t of
        Right q -> turn (q,t:zs) ts
        Left  u -> (p,u:zs)
    cmp :: Point -> Turn -> Either Turn Point
    cmp (x,y) (L,a) = if y==y0 && x > x0 && x-a < x0 then Left (L,x-x0) else Right (x-a,y)
    cmp (x,y) (R,a) = if y==y0 && x < x0 && x+a > x0 then Left (R,x0-x) else Right (x+a,y)
    cmp (x,y) (U,a) = if x==x0 && y < y0 && y+a > y0 then Left (U,y0-y) else Right (x,y+a)
    cmp (x,y) (D,a) = if x==x0 && y > y0 && y-a < y0 then Left (D,y-y0) else Right (x,y-a)

intersections :: [Segment] -> [Segment] -> [Point]
intersections as bs = catMaybes [intersect a b | a <- as, b <- bs]
  where
    intersect (V x (y0,y1)) (H y (x0,x1))
      | x0 < x && x < x1 && y0 < y && y < y1 = Just (x,y)
    intersect h@(H _ _) v@(V _ _) = intersect v h
    intersect _ _ = Nothing

solve1 :: [Wire] -> Int
solve1 wires = minimum $ map manhattan $ intersections a b
  where [a,b] = map segments wires
        manhattan (x,y) = abs x + abs y

solve2 :: [Wire] -> Int
solve2 wires = minimum $ map (flip combine wires) $ intersections a b
  where [a,b] = map segments wires
        combine :: Point -> [Wire] -> Int
        combine point = sum . map (steps point)
        steps :: Point -> Wire -> Int
        steps p = sum . map snd . path p

parse :: String -> Wire
parse = map turn . splitOn ","
  where turn (x:xs) = (twist x, read xs)
        twist 'L' = L
        twist 'R' = R
        twist 'U' = U
        twist 'D' = D

pair (f,g) x = (f x, g x)

main = print . pair (solve1,solve2) . map parse . lines =<< getContents

tests = [["R75,D30,R83,U83,L12,D49,R71,U7,L72"       
         ,"U62,R66,U55,R34,D71,R55,D58,R83"],
         ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
         ,"U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]]
test1 = map (solve1 . map parse) tests == [159,135]
test2 = map (solve2 . map parse) tests == [610,410]
