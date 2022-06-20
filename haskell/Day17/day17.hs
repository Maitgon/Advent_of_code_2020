-- Day 17.

import qualified Data.Map as M
type Position = (Int, Int, Int)
type Position2 = (Int, Int, Int, Int)

main :: IO ()
main = do
    -- Input reading.
    input <- lines <$> readFile "input.txt"
    let height = length input
    let width = length $ head input
    let matrix = [(i,j,0) | i <- [0..height-1], j <- [0..width-1], (input !! i) !! j == '#']
    let matrix2 = [(i,j,0,0) | i <- [0..height-1], j <- [0..width-1], (input !! i) !! j == '#']

    -- Sol to part 1.
    let sol1 = length $ iterateMap3 6 matrix
    print sol1

    -- Sol to part 2.
    let sol2 = length $ iterateMap4 6 matrix2
    print sol2


createMap3 :: Position -> M.Map Position Int
createMap3 (x,y,z) = M.fromList [((x+i,y+j,z+k), 1) | i <- [-1,0,1], j <- [-1,0,1], k <- [-1,0,1], not (i == 0 && j == 0 && k == 0)]

createMap4 :: Position2 -> M.Map Position2 Int
createMap4 (x,y,z,w) = M.fromList [((x+i,y+j,z+k,w+l), 1) | i <- [-1,0,1], j <- [-1,0,1], k <- [-1,0,1], l <- [-1,0,1], not (i == 0 && j == 0 && k == 0 && l == 0)]

iterateMap3 :: Int -> [Position] -> [Position] 
iterateMap3 0 pos = pos
iterateMap3 n pos = iterateMap3 (n-1) $ M.keys potentialPos
    where potentialPos = M.filterWithKey (\k x -> x==3 || x==2 && k `elem` pos) $ foldl1 (M.unionWith (+)) $ map createMap3 pos

iterateMap4 :: Int -> [Position2] -> [Position2] 
iterateMap4 0 pos = pos
iterateMap4 n pos = iterateMap4 (n-1) $ M.keys potentialPos
    where potentialPos = M.filterWithKey (\k x -> x==3 || x==2 && k `elem` pos) $ foldl1 (M.unionWith (+)) $ map createMap4 pos