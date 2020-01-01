import Data.List.Split
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Control.Arrow

type T a = Map a [a]

tree :: Ord a => [(a,a)] -> T a
tree = foldr f M.empty
  where f (a,b) m = M.insertWith (++) a [b]
                  $ M.insertWith (++) b [] m

look :: Ord a => a -> T a -> [a]
look a = fromJust . M.lookup a

depths :: Ord a => a -> T a -> [[a]]
depths root m = go [root]
  where
    go [] = []
    go xs = ys : go ys
      where ys = concatMap (flip look m) xs

orbits :: T String -> Int
orbits = sum . zipWith (*) [1..] . map length . depths "COM"

type G a = [(a,[a])]
type P a = [[(a,a)]]

connect :: Eq a => a -> a -> G a -> P a
connect x y g
    | x == y    = [[]]
    | otherwise = [(x,t):p | t <- f x, p <- connect t y g]
    where f = fromJust . flip lookup g

path :: Eq a => a -> a -> G a -> [a]
path a b = map fst . head . connect a b

dropCommon :: Eq a => ([a],[a]) -> ([a],[a])
dropCommon (xs,[]) = (xs,[])
dropCommon ([],ys) = ([],ys)
dropCommon (xxs@(x:xs),yys@(y:ys))
  | x == y = dropCommon (xs,ys)
  | otherwise = (xxs,yys)

transfers :: T String -> Int
transfers = uncurry (+) . cross length . dropCommon 
          . pair (cross (path "COM") ("YOU","SAN"))
          . M.assocs

pair (f,g) x = (f x, g x)
cross f (a,b) = (f a, f b)

parse :: [String] -> [(String,String)]
parse = map (tuple . splitOn ")")
  where tuple [a,b] = (a,b)

main = interact $ show . (orbits &&& transfers) . tree . parse . lines

test = (test1,test2) == (42,4)
  where
    example =
       [["COM)B"
        ,"B)C"
        ,"C)D"
        ,"D)E"
        ,"E)F"
        ,"B)G"
        ,"G)H"
        ,"D)I"
        ,"E)J"
        ,"J)K"
        ,"K)L"],
        ["K)YOU"
        ,"I)SAN"]]
    test1 = orbits $ tree $ parse (head example)
    test2 = transfers $ tree $ parse (concat example)
