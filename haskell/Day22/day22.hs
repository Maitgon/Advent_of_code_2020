-- Day 22.

import Data.List.Split



main = do
    -- Input reading.
    [_:deck1, _:deck2] <- map (map (read :: String -> Int) . lines) . splitOn "\n\nPlayer 2:" <$> readFile "input.txt"
    print (deck1, deck2)

    -- Sol to part 1.
    let winDeck = play deck1 deck2
    let sol1 = sum $ zipWith (*) (reverse winDeck) [1..]
    print sol1

    -- Sol to part 1.
    let winDeck2 = snd $ playRecursion deck1 deck2 [] []
    let sol2 = sum $ zipWith (*) (reverse winDeck2) [1..]
    print sol2


play :: [Int] -> [Int] -> [Int]
play [] ys = ys
play xs [] = xs
play (x:xs) (y:ys) = if x > y
                     then play (xs ++ [x,y]) ys
                     else play xs (ys ++ [y,x])

playRecursion :: [Int] -> [Int] -> [[Int]] -> [[Int]] -> (Int, [Int])
playRecursion [] ys _ _ = (2,ys)
playRecursion xs [] _ _ = (1,xs)
playRecursion (x:xs) (y:ys) prevX prevY
    | x <= length xs && y <= length ys = if fst (playRecursion (take x xs) (take y ys) [] []) == 1
                                         then playRecursion (xs ++ [x,y]) ys ((x:xs):prevX) ((y:ys):prevY)
                                         else playRecursion xs (ys ++ [y,x]) ((x:xs):prevX) ((y:ys):prevY)
    | (x:xs) `elem` prevX || (y:ys) `elem` prevY = (1, x:xs)
    | x > y = playRecursion (xs ++ [x,y]) ys ((x:xs):prevX) ((y:ys):prevY)
    | otherwise = playRecursion xs (ys ++ [y,x]) ((x:xs):prevX) ((y:ys):prevY)