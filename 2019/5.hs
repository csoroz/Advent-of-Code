{-# LANGUAGE FlexibleContexts #-}
import Data.List
import Data.List.Split
import Data.Array.IO

type Intcode = [Int]

data Parameter = Pos Int | Imm Int deriving Show

execute :: IOUArray Int Int -> [Int] -> [Int] -> Int -> IO [Int]
execute a inputs outputs i = uncurry go . decode =<< readArray a i
  where
    go 99 = return . const (reverse outputs)
    go 1 = compute (+)
    go 2 = compute (*)
    go 3 = input
    go 4 = output
    go 5 = jump (/=)
    go 6 = jump (==)
    go 7 = cmp (<)
    go 8 = cmp (==)
    decode instr = (op, map mode [m1,m2,m3])
      where
        mode 0 = Pos
        mode 1 = Imm
        (modes,op) = divMod instr 100
        (mm,m1) = divMod modes 10
        (m3,m2) = divMod mm 10
    write x (Pos i) = writeArray a i x
    fetch (Pos i) = readArray a i
    fetch (Imm i) = return i
    parameter m i = m <$> readArray a i
    fetchParam m i = fetch =<< parameter m i
    addr m k = parameter (m!!(k-1)) (i+k)
    params m n = sequence $ zipWith fetchParam m [i+1..i+n]
    next = execute a inputs outputs
    input m = do
        write (head inputs) =<< addr m 1
        execute a (tail inputs) outputs (i+2)
    output m = do
        x <- head <$> params m 1
        execute a inputs (x:outputs) (i+2)
    jump p m = do
        [x,y] <- params m 2
        next $ if p x 0 then y else i+3
    cmp c m = do
        [x,y] <- params m 2
        write (fromEnum $ c x y) =<< addr m 3
        next $ i+4
    compute op m = do
        [x,y] <- params m 2
        write (op x y) =<< addr m 3
        next $ i+4

diagnostic :: [Int] -> Intcode -> IO [Int]
diagnostic inputs code = do
    a <- newListArray (0, length code - 1) code :: IO (IOUArray Int Int)
    execute a inputs [] 0

parse :: String -> Intcode
parse = map read . splitOn ","

main = do code <- parse <$> getContents
          print =<< diagnostic [1] code
          print =<< diagnostic [5] code

tests = do
    let t1 = [3,9,8,9,10,9,4,9,99,-1,8]
    let t2 = [3,9,7,9,10,9,4,9,99,-1,8]
    let t3 = [3,3,1108,-1,8,3,4,3,99]
    let t4 = [3,3,1107,-1,8,3,4,3,99]
    let t5 = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
    let t6 = [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
    let t7 = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
              1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
              999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
    rs <- concat <$> sequence
        [diagnostic [7] t1
        ,diagnostic [8] t1
        ,diagnostic [7] t2
        ,diagnostic [8] t2
        ,diagnostic [7] t3
        ,diagnostic [8] t3
        ,diagnostic [7] t4
        ,diagnostic [8] t4
        ,diagnostic [0] t5
        ,diagnostic [8] t5
        ,diagnostic [0] t6
        ,diagnostic [8] t6
        ,diagnostic [7] t7
        ,diagnostic [8] t7
        ,diagnostic [9] t7
        ]
    return $ rs == [0,1,1,0,0,1,1,0,0,1,0,1,999,1000,1001]
