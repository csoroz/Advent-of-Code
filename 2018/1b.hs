import qualified Data.Set as Set
import Data.Maybe
import Data.List

main = interact $ show . execute . cycle . map parse . lines
  where execute = snd . fromJust . find p . scanl g (Set.empty,0)
          where g (s,t) (o,x) = (Set.insert t s, o t x)
                p (s,t) = Set.member t s
        parse (x:xs) = (op x, read xs)
          where
            op '-' = (-)
            op '+' = (+)
