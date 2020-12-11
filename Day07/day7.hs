-- Day 7.

import Data.List.Split ( splitOn )
import Data.Char ( digitToInt )
import qualified Data.Map as M
import Data.List (nub)

type Bag = (Int, String)

main :: IO ()
main = do
    -- Input reading
    input <- readFile "input.txt"
    let parse1 = map (splitOnAnyOf [" contain ", " bags.", " bags, ", " bags", " bag.", " bag, "]) $ lines input
    let parse2 = map (parse_2 . delete "") parse1
    let mapeo = M.fromList parse2

    -- Sol to part 1.
    let sol1 = getGood parse2 "shiny gold"
    putStrLn $ "The solution to part 1 is: " ++ show ((length . nub)  sol1)

    -- Sol to part 2.
    let sol2 = getBags mapeo "shiny gold"
    putStrLn $ "The solution to part 2 is: " ++ show sol2


getGood :: [(String, [Bag])] -> String  -> [String]
getGood bags target = newTarget ++ (newTarget >>= getGood bags)
    where newTarget = [fst bag | bag <- bags, target `elem` map snd (snd bag)]

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:xs)
    | x == y = delete x xs
    | otherwise = y : delete x xs

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl (\ys d -> ys >>= splitOn d) [xs] ds

parse_2 :: [String] -> (String, [Bag])
parse_2 (x:xs) = if head (head xs) == 'n' then (x, []) else (x, map (\(x:_:xs) -> (digitToInt x, xs)) xs)

getBags :: M.Map String [Bag] -> String -> Int
-- This is a sum of the bags and the scalar product of the bags contained and the number of those bags, it can be reduced as m*(n+1)
-- getBags state target = sum [fst x | x <- bags] + sum (zipWith (*) [fst x | x <- bags] [getBags state (snd x)| x <- bags])
getBags state target = sum [fst x * (getBags state (snd x) + 1)| x <- bags]
    where bags = state M.! target

-- Used for debugging
getElems :: String -> [(String, [Bag])] -> [Bag]
getElems _ [] = []
getElems color ((x,y):xs) = if x == color then y else getElems color xs