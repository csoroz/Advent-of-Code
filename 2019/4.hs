import Data.Digits
import Data.List

adjacent p ds = any (p.length) (group ds)
nondecreasing ds = and $ zipWith (<=) ds (tail ds)

valid :: (Int -> Bool) -> Int -> Bool
valid p = prop . digits 10
  where prop ds = adjacent p ds && nondecreasing ds

count p = length . filter (valid p)

pair (f,g) x = (f x, g x)

solve = pair (count (>1), count (==2))

main = print $ solve [134564..585159]
