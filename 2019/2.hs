import Data.List
import Data.List.Split
import Data.Array.IO

type Intcode = [Int]

execute a i = readArray a i >>= go
  where
    go 99 = readArray a 0
    go 1 = compute (+)
    go 2 = compute (*)
    compute op = do
        x <- readArray a (i+1)
        y <- readArray a (i+2)
        z <- readArray a (i+3)
        v <- readArray a x
        w <- readArray a y
        writeArray a z (op v w)
        execute a (i+4)

calc :: (Int,Int) -> Intcode -> IO Int
calc (noun,verb) code = do
    a <- newListArray (0, length code - 1) code :: IO (IOUArray Int Int)
    writeArray a 1 noun
    writeArray a 2 verb
    execute a 0

solve :: Int -> Intcode -> IO Int
solve output code = do
    let xs = [(n,v) | n <- [0..99], v <- [0..99]]
    ys <- sequence $ map (flip calc code) xs
    let Just ((noun,verb),_) = find ((==output).snd) (zip xs ys)
    return (100*noun + verb)

parse :: String -> Intcode
parse = map read . splitOn ","

main = do code <- parse <$> getContents
          print =<< calc (12,2) code
          print =<< solve 19690720 code
