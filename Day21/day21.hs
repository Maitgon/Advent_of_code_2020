-- Day 21.

import qualified Data.Map as M
import Data.List.Split ( splitOn )
import qualified Data.List as L


type Allergens = String

main :: IO ()
main = do
    -- Input reading.
    input <- map (map (splitOnAnyOf [", ", " "]) . splitOn " (contains ". init) . lines <$> readFile "input.txt"
    print input

    -- Sol to part 1.
    let assign = M.unionsWith L.intersect $ map createMap input
    let alerg = M.keys assign
    let posible = M.elems assign
    let get1 = assignFields posible $ head [x | x <- posible, length x == 1]
    let allElems = concatMap head input
    let sol1 = length allElems - length (L.intersect allElems $ concat get1)
    let cosa = concat get1
    print sol1

    -- Sol to part 2.
    let sol2 = tail $ foldl1 (++) $ concatMap (map (',':)) get1
    print sol2



splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl (\ys d -> ys >>= splitOn d) [xs] ds

createMap :: [[String]] -> M.Map String [String]
createMap [_, []] = M.empty
createMap [pos, x:xs] = M.insert x pos (createMap [pos, xs])

countElems :: Eq a => [a] -> [a] -> Int
countElems xs elems = sum [if x `elem` elems then 1 else 0 | x <- xs]

-- Taken from day 16.

assignFields :: [[String]] -> [String] -> [[String]]
assignFields poss del = if all (==1) (map length poss)
                        then poss
                        else assignFields
                             (delete poss del)
                             (getElems poss)


delete :: Eq a => [[a]] -> [a] -> [[a]]
delete [] del = []
delete (x:xs) del = if length x /= 1
                    then remove del x : delete xs del
                    else x : delete xs del

remove :: Eq a => [a] -> [a] -> [a]
remove rs ls = foldl remove' ls rs
      where remove' ls x = filter (/= x) ls

getElems :: [[a]] -> [a]
getElems poss = [head elem | elem <- poss , length elem == 1]