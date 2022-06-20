-- Day 9.

main = do
    -- Input reading.
    input <- readFile "input.txt"
    let vector = (map (read :: String -> Int) . lines) input

    -- Sol to part 1.
    let sol1 = part1 vector
    print sol1

    -- Sol to part 2.
    let sol2 = part2 vector sol1 0
    print sol2



part1 :: [Int] -> Int
part1 (x:xs) = if null sums then (x:xs) !! 25 else part1 xs
                                where sums = [((x:xs) !! i) + ((x:xs) !! j) | i <- [0..24], j <- [0..24], i /= j, ((x:xs) !! i) + ((x:xs) !! j) == (x:xs) !! 25]

part2 :: [Int] -> Int -> Int -> Int
part2 (x:xs) target elems
    | sum seq == target = minimum seq + maximum seq
    | sum seq < target = part2 (x:xs) target (elems+1)
    | sum seq > target = part2 xs target 1
        where seq = take elems (x:xs)