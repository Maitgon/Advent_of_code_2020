-- Day8.

import Data.Array
import Data.Char
type Inst = (String, Int)

main = do
    -- Input reading.
    content <- readFile "input.txt"
    let input = map (span (\x -> not (isDigit x) && x /= '-') . delete ' ' . delete '+') $ lines content
    let input2 = map (\(x,y) -> (x, (read :: String -> Int) y)) input
    let len = length input
    let game = array (0, len -1) [(i, input2 !! i) | i <- [0..len-1]]

    -- Sol to part 1.
    let sol1 = runGame game
    putStrLn $ "The solution to part 1 is: " ++ show sol1

    -- Sol to part 2.
    let sol2 = runGame2 game 0 len
    putStrLn $ "The solution to part 2 is: " ++ show sol2

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:xs)
    | x == y = xs
    | otherwise = y : delete x xs

runGame :: Array Int Inst -> Int
runGame game = runGameAux game 0 [] 0

runGameAux :: Array Int Inst -> Int -> [Int] -> Int -> Int
runGameAux game i visited acc
    | i `elem` visited = acc
    | fst (game ! i) == "nop" = runGameAux game (i+1) (i:visited) acc
    | fst (game ! i) == "acc" = runGameAux game (i+1) (i:visited) (acc + snd (game ! i))
    | fst (game ! i) == "jmp" = runGameAux game (i + snd (game ! i)) (i:visited) acc

runGame2 :: Array Int Inst -> Int -> Int -> Int
runGame2 game i size
    | fst (game ! i) == "acc" = runGame2 game (i+1) size
    | fst (game ! i) == "nop" = if snd res then fst res else runGame2 game (i+1) size
    | fst (game ! i) == "jmp" = if snd res' then fst res' else runGame2 game (i+1) size
                                where res = runGame2Aux (game // [(i,new)]) 0 [] 0 size
                                      new = ("jmp", snd (game ! i))
                                      res' = runGame2Aux (game // [(i,new')]) 0 [] 0 size
                                      new' = ("nop", snd (game ! i))



runGame2Aux :: Array Int Inst -> Int -> [Int] -> Int -> Int -> (Int, Bool)
runGame2Aux game i visited acc size
    | i >= size = (acc, True)
    | i `elem` visited = (acc, False)
    | fst (game ! i) == "nop" = runGame2Aux game (i+1) (i:visited) acc size
    | fst (game ! i) == "acc" = runGame2Aux game (i+1) (i:visited) (acc + snd (game ! i)) size
    | fst (game ! i) == "jmp" = runGame2Aux game (i + snd (game ! i)) (i:visited) acc size