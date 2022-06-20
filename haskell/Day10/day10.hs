-- Day 10.

import Data.List

main = do
    -- Input reading
    vector <- fmap (map (read :: String -> Int) . lines) (readFile "input.txt")
    print $ sort vector

    -- Solution to part 1.
    let sorted = sort vector
    let sol1 = part1 (0:sorted) (0,1)
    putStrLn $ "The solution to the part 1 is: " ++ show (uncurry (*) sol1)

    -- Solution to part 2.
    let sol2 = product $ map posibilities $ part2 (0:sorted) 1
    putStrLn $ "The solution to the part 2 is: " ++ show sol2



part1 :: [Int] -> (Int, Int) -> (Int, Int)
part1 [_] p = p
part1 (x:y:xs) (p1, p2)
    | y - x == 1 = part1 (y:xs) (p1+1, p2)
    | y - x == 3 = part1 (y:xs) (p1, p2+1)

posibilities :: Int -> Int
posibilities 5 = 7
posibilities n = 2^ max (n-2) 0

part2 :: [Int] -> Int -> [Int]
part2 [_] long = [long]
part2 (x:y:xs) long
    | y - x == 1 = part2 (y:xs) (long+1)
    | y - x == 3 = long : part2 (y:xs) 1
