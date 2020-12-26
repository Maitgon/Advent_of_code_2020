-- Day 24.

import Data.List
import qualified Data.Map as M

type Instruction = String
type Tile = (Int, Int)

main = do
    -- Input reading.
    input <- map getInstructions . lines <$> readFile "input.txt"
    
    -- Sol to part 1.
    let sol1 = length $ flipTiles input
    print sol1

    -- Sol to part 2.
    let sol2 = length $ iter 100 $ flipTiles input
    print sol2


getInstructions :: String -> [Instruction]
getInstructions [] = []
getInstructions [x] = [[x]]
getInstructions (x:y:xs)
    | x == 's' || x == 'n' = [x,y] : getInstructions xs
    | x == 'e' || x == 'w' = [x] : getInstructions (y:xs)

getTile :: [Instruction] -> Tile -> Tile
getTile [] tile = tile
getTile (x:xs) (z,y)
    | x == "e" = getTile xs (z+1,y)
    | x == "se" = getTile xs (z+1,y-1)
    | x == "sw" = getTile xs (z,y-1)
    | x == "w" = getTile xs (z-1,y)
    | x == "nw" = getTile xs (z-1,y+1)
    | x == "ne" = getTile xs (z,y+1)

flipTiles :: [[Instruction]] -> [Tile]
flipTiles xs = flipTilesAux xs []

flipTilesAux :: [[Instruction]] -> [Tile] -> [Tile]
flipTilesAux [] tiles = tiles
flipTilesAux (x:xs) tiles
    | new `notElem` tiles = flipTilesAux xs (new:tiles)
    | otherwise = flipTilesAux xs (delete new tiles)
        where new = getTile x (0,0)

getNeigh :: Tile -> M.Map Tile Int
getNeigh (x,y) = M.fromList [((x+1,y),1), ((x+1,y-1),1), ((x,y-1),1), ((x-1,y),1), ((x-1,y+1),1), ((x,y+1),1)]

getAllBlacks :: [Tile] -> M.Map Tile Int
getAllBlacks tiles = M.fromList $ zip tiles [0..0]

iter :: Int -> [Tile] -> [Tile]
iter 0 tiles = tiles
iter n tiles = iter (n-1) $ M.keys nextTiles
    where nextTiles = M.filterWithKey (\k x -> (x==2 && k `notElem` tiles) || (x==2 || x==1) && k `elem` tiles) 
                      $ foldl1 (M.unionWith (+)) 
                      $ map getNeigh tiles ++ [getAllBlacks tiles]

        