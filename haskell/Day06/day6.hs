-- Day 6.

import Data.List.Split ( splitOn )
import qualified Data.Set as Set
import Data.List ( intersect )

main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = splitOn "\n\n" content
    let groups = map (filter (/= '\n')) $ input
    let groups2 = map lines input
    
    -- Part 1.
    let sol1 = map (Set.size . Set.fromList) groups
    putStrLn $ "The solution to part 1 is: " ++ show (sum sol1)

    -- Part 2.
    let sol2 = map (length . foldr1 intersect) groups2
    putStrLn $ "The solution to part 2 is: " ++ show (sum sol2)