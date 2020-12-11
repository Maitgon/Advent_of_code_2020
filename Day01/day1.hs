-- Day 1

main = do
    -- Input reading.
    content <- readFile "input.txt"
    let vector = map (read :: String -> Int) (lines content)

    -- Sol to part 1.
    let sol1 = head [x*y | x <- vector, y <- vector, x + y == 2020]
    putStrLn $ "The solution to part 1 is: " ++ show sol1

    -- Sol to part 2.
    let sol2 = head [x*y*z | x <- vector, y <- vector, z <- vector, x + y + z == 2020]
    putStrLn $ "The solution to part 2 is: " ++ show sol2