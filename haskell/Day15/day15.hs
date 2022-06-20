-- Day 15.

import qualified Data.Map as M
import Data.Maybe ( isNothing )

main :: IO ()
main = do
    -- Input reading.
    let input = [7,12,1,0,16,2]

    -- Sol to part 1.
    let sol1 = solve input M.empty 1 0 2020
    putStrLn $ "The solution to part 1 is: " ++ show sol1

    -- Sol to part 2.
    let sol2 = solve input M.empty 1 0 30000000
    putStrLn $ "The solution to part 2 is: " ++ show sol2

solve :: [Int] -> M.Map Int Int -> Int -> Int -> Int -> Int
solve [] mem turn val stop
    | isNothing (mem M.!? val) = if turn /= stop - 1 then solve [] (M.insert val turn mem) (turn+1) 0 stop else 0
    | otherwise = if turn /= stop - 1 then solve [] (M.insert val turn mem) (turn+1) (turn - mem M.! val) stop else turn - mem M.! val
solve [x] mem turn _ stop
    | isNothing (mem M.!? x) = solve [] (M.insert x turn mem) (turn+1) 0 stop
    | otherwise = solve [] (M.insert x turn mem) (turn+1) (turn - mem M.! x) stop
solve (x:xs) mem turn _ stop = solve xs (M.insert x turn mem) (turn+1) 0 stop