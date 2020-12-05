-- Day5.

import Data.List

main = do
    -- input reading
    input <- readFile "input.txt"
    let vector = map fromBinary (lines input)

    -- sol to part 1.
    let ids = map (\(x,y) -> x*8 + y) vector
    putStrLn $ "The solution to part 1 is: " ++ show (maximum ids)

    -- sol to part 2.
    let missing = filter (\x -> x<900 && x>100) $ [0..(127*8 + 7)] \\ ids --Kind os a lazy solution.
    putStrLn $ "The solution to part 2 is: " ++ show (head missing)

fromBinary :: String -> (Int, Int)
fromBinary xs = fromBinary' xs 6 2

fromBinary' :: String -> Int -> Int -> (Int, Int)
fromBinary' [] _ _= (0, 0)
fromBinary' (x:xs) row col
    | x == 'F' = suma' (0, 0) (fromBinary' xs (row-1) col)
    | x == 'B' = suma' (2^row, 0) (fromBinary' xs (row-1) col)
    | x == 'L' = suma' (0, 0) (fromBinary' xs row (col-1))
    | x == 'R' = suma' (0, 2^col) (fromBinary' xs row (col-1))


suma' :: (Int, Int) -> (Int, Int) -> (Int, Int)
suma' (a, b) (c, d) = (a+c,b+d)