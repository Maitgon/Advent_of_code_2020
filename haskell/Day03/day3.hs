-- Day 3.

import Data.Array

main = do
    -- Input reading.
    content <- readFile "input.txt"
    let input = lines content
    let width = length (head input)
    let heigth = length input
    let matrix = array ((0,0), (heigth-1, width-1)) [((i,j), (input !! i) !! j) | i <- [0..heigth-1], j <- [0..width-1]]

    -- sol to part 1.
    let sol1 = countTrees_zx_zy matrix 0 0 heigth width (1,3)
    putStrLn $ "The solution to part 1 is: " ++ show sol1

    -- sol to part 2.
    let sol2 = product [countTrees_zx_zy matrix 0 0 heigth width moves | moves <- [(1,1), (1,3), (1,5), (1,7), (2,1)]]
    putStrLn $ "The solution to part 2 is: " ++ show sol2

--                  array                    current x    current y    top x    top y     move x    move y    num arboles
countTrees_zx_zy :: Array (Int, Int) Char -> Int       -> Int       -> Int   -> Int   -> (Int    , Int    )-> Int
countTrees_zx_zy a x y tx ty (zx, zy)
    | x >= tx = 0
    | a ! (x, y) == '#' = 1 + countTrees_zx_zy a (x+zx) (mod (y+zy) ty) tx ty (zx, zy)
    | otherwise = countTrees_zx_zy a (x+zx) (mod (y+zy) ty) tx ty (zx, zy)