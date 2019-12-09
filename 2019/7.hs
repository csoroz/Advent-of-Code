{-# LANGUAGE FlexibleContexts, TupleSections #-}
import Data.List
import Data.List.Split
import Data.Array.IO

type Intcode = ([Int],Int)

intcode xs = (xs, length xs)

data Parameter = Pos Int | Imm Int deriving Show

execute :: IOUArray Int Int -> [Int] -> Int -> IO (Int,Int)
execute a inputs i = uncurry go . decode =<< readArray a i
  where
    go 99 = halt
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
    halt _ = (,-1) <$> readArray a 0
    write x (Pos i) = writeArray a i x
    fetch (Pos i) = readArray a i
    fetch (Imm i) = return i
    parameter m i = m <$> readArray a i
    fetchParam m i = fetch =<< parameter m i
    addr m k = parameter (m!!(k-1)) (i+k)
    params m n = sequence $ zipWith fetchParam m [i+1..i+n]
    next = execute a inputs
    input m = do
        write (head inputs) =<< addr m 1
        execute a (tail inputs) (i+2)
    output m = do
        x <- head <$> params m 1
        return (x,i+2)
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

diagnostic :: Intcode -> [Int] -> IO (Int,Int)
diagnostic (code,n) inputs = do
    a <- newListArray (0,n-1) code :: IO (IOUArray Int Int)
    execute a inputs 0

reset :: Intcode -> Int -> IO [IOUArray Int Int]
reset (code,n) k = sequence <$> replicate k $ newListArray (0,n-1) code

thruster :: Intcode -> Int -> [Int] -> IO Int
thruster code x [p0,p1,p2,p3,p4] = reset code 5 >>= run
  where 
    run [a,b,c,d,e] = go x $ replicate 5 (-1)
      where
        go x0 [i1,i2,i3,i4,i5] = do
            (x1,i1') <- execute a (f i1 p0 x0) (g i1)
            (x2,i2') <- execute b (f i2 p1 x1) (g i2)
            (x3,i3') <- execute c (f i3 p2 x2) (g i3)
            (x4,i4') <- execute d (f i4 p3 x3) (g i4)
            (x5,i5') <- execute e (f i5 p4 x4) (g i5)
            if i5'<0 then return x0 else go x5 [i1',i2',i3',i4',i5']
            where f i p x = if i<0 then [p,x] else [x]
                  g i = if i<0 then 0 else i

solve :: [Int] -> Intcode -> IO Int
solve xs code = fmap maximum $ sequence $ map (thruster code 0) (permutations xs)

parse :: String -> Intcode
parse = intcode . map read . splitOn ","

pair (f,g) x = [f x, g x]

main = print =<< sequence . pair (solve [0..4], solve [5..9]) =<< parse <$> getContents

-- [929800,15432220]

tests = do
    let t1 = intcode [3,9,8,9,10,9,4,9,99,-1,8]
    let t2 = intcode [3,9,7,9,10,9,4,9,99,-1,8]
    let t3 = intcode [3,3,1108,-1,8,3,4,3,99]
    let t4 = intcode [3,3,1107,-1,8,3,4,3,99]
    let t5 = intcode [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
    let t6 = intcode [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
    let t7 = intcode [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
    rs <- map fst <$> sequence
        [diagnostic t1 [7]
        ,diagnostic t1 [8]
        ,diagnostic t2 [7]
        ,diagnostic t2 [8]
        ,diagnostic t3 [7]
        ,diagnostic t3 [8]
        ,diagnostic t4 [7]
        ,diagnostic t4 [8]
        ,diagnostic t5 [0]
        ,diagnostic t5 [8]
        ,diagnostic t6 [0]
        ,diagnostic t6 [8]
        ,diagnostic t7 [7]
        ,diagnostic t7 [8]
        ,diagnostic t7 [9]
        ]
    return $ rs == [0,1,1,0,0,1,1,0,0,1,0,1,999,1000,1001]

tt = do
    let t1 = intcode [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
    let t2 = intcode [3,23,3,24,1002,24,10,24,1002,23,-1,23,
                      101,5,23,23,1,24,23,23,4,23,99,0,0]
    let t3 = intcode [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                      1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
    let t4 = intcode [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                      27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
    let t5 = intcode [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                      -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                      53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
    rs <- sequence
        [thruster t1 0 [4,3,2,1,0]
        ,thruster t2 0 [0,1,2,3,4]
        ,thruster t3 0 [1,0,4,3,2]
        ,thruster t4 0 [9,8,7,6,5]
        ,thruster t5 0 [9,7,8,5,6]
        ]
    return $ rs == [43210,54321,65210,139629729,18216]
