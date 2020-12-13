-- Day 13

import Data.List.Split
import Data.List
import Data.Ord

main = do
    -- Input reading.
    input <- splitOnAnyOf [",", "\n"] <$> readFile "input.txt"
    let yourID = read (head input) :: Int
    let buses = map (read :: String -> Int) $ tail $ filter (\x -> x /= "x" && x/= "" ) input
    
    -- Sol to part 1.
    let (minIndex, minWait) = minimumBy (comparing snd) (zip [0..] $ map (\x -> x - mod yourID x) buses)
    let sol1 = minWait * (buses !! minIndex)
    putStrLn $ "The solution to part 1 is: " ++ show sol1

    -- Sol to part 2.
    let buses2 = map (\(x,y) -> (read x :: Int,y)) $ filter (\(x,_) -> x /= "x" && x/= "" ) $  zip (tail input) [0..]
    let ni = map fst buses2
    let n = product ni
    let yi = [div n i | i <- ni]
    let zi = zipWith modInv yi ni
    let ai = zipWith mod (map (negate . snd) buses2) ni
    let asol = sum $ zipWith3 (\x y z -> x*y*z) ai yi zi
    let mcm = product ni
    let sol2 = mod asol mcm
    putStrLn $ "The solution to part 2 is: " ++ show sol2


splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl (\ys d -> ys >>= splitOn d) [xs] ds

-- Solve x in a*x = 1 (mod n)
modInv :: Int -> Int -> Int
modInv a n = x
    where (x, _, _) = extGCD a n

extGCD :: Int -> Int -> (Int,Int,Int)
extGCD 0 0 = error "extGCD(0,0) is undefined"
extGCD a 0 = (1,0,a)                             -- Base case
extGCD a b = let (q,r) = a `divMod` b            -- q and r of a/b
                 (c,x,y) = extGCD b r            -- Recursive call
             in  (x,c-q*x, y)                    -- Recursive results