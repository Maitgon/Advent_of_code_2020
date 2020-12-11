-- Day 2.

main = do
    -- Input reading.
    content <- readFile "input.txt"
    let input = map prettyRead (lines content)
    
    -- Solution to part 1.
    let sol1 = length [0 | x <- input, isValid (read' (x !! 0)) (read' (x !! 1)) (head (x !! 2)) (x !! 3) 0]
    putStrLn $ "The soluction to part 1 is: " ++ show sol1

    -- Solution to part 2.
    let sol2 = length [0 | x <- input, isValid2 (read' (x !! 0)) (read' (x !! 1)) (head (x !! 2)) (x !! 3)]
    putStrLn $ "The soluction to part 2 is: " ++ show sol2



prettyRead :: String -> [String]
prettyRead xs = words (map substitute xs )
    where substitute '-' = ' '
          substitute ':' = ' '
          substitute c = c

isValid :: Int -> Int -> Char -> String -> Int -> Bool
isValid low high _ [] cont = low <= cont && cont <= high
isValid low high c (x:xs) cont = if c == x then isValid low high c xs (cont+1) else isValid low high c xs cont

read' :: String -> Int
read' = read :: String -> Int

isValid2 :: Int -> Int -> Char -> String -> Bool
isValid2 low high c xs = xor (xs !! (low-1) == c) (xs !! (high-1) == c)

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a