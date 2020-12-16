-- Day 16.

import Data.List.Split ( splitOn )
import Control.Monad ( guard )

type Field = (String, Int, Int, Int, Int)

main :: IO ()
main = do
    [input1, input2, input3] <- splitOn "\n\n" <$> readFile "input.txt"
    let fields = map (parse1 . splitOnAnyOf [": ", "-", " or "]) $ lines input1
    let myTicket = map (read :: String -> Int) $ splitOn "," $ last $ lines input2
    let neTicket = map (map (read :: String -> Int) . splitOn ",") $ tail $ lines input3

    -- Sol to part 1.
    let sol1 = sum $ filter (notValidAll fields) $ concat neTicket
    print sol1

    -- Sol to part 2.
    let posfields = transpose $ [ tickets | tickets <- neTicket, all (not . notValidAll fields) tickets]
    let valid = [[(\(x,_,_,_,_) -> x) field | field <- fields, isField ns field] | ns <- posfields]
    let cu1 = head [x | x <- valid, length x == 1]
    let sol2 = product $ getDepartures $ zip (concat $ assignFields valid cu1) myTicket
    print sol2

    -- debug
    --let solbug = assignFields valid 0 []
    let solbug = map length valid
    let solbug2 = map length $ delete valid cu1
    print solbug
    print solbug2


parse1 :: [String] -> Field
parse1 [x1,x2,x3,x4,x5] = (x1, read x2 :: Int, read x3 :: Int, read x4 :: Int, read x5 :: Int)

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl (\ys d -> ys >>= splitOn d) [xs] ds

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)


isValid :: Int -> Field -> Bool
isValid n (_,x2,x3,x4,x5) = n >= x2 && n <= x3 || n >= x4 && n <= x5

notValidAll :: [Field] -> Int -> Bool
notValidAll xs n = not $ any (isValid n) xs

isField :: [Int] -> Field -> Bool
isField ns x = all (flip isValid x) ns

{- Backtracking version (which obviously doesn't work here).
assignFields :: [[String]] -> Int -> [String] -> [String]
assignFields fields deep used = do
    field <- remove used $ fields !! deep
    guard $ field `notElem` used && (remove used (fields !! deep) /= [])
    if deep == length fields - 1
    then field:used
    else assignFields fields (deep+1) (field:used)
-}


remove :: Eq a => [a] -> [a] -> [a]
remove rs ls = foldl remove' ls rs
      where remove' ls x = filter (/= x) ls

prueba :: [Integer]
prueba = do
    a <- []
    if a == ()
    then [1]
    else [0]

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

getElems :: [[a]] -> [a]
getElems poss = [head elem | elem <- poss , length elem == 1]

getDepartures :: [(String, Int)] -> [Int]
getDepartures [] = []
getDepartures ((x,y):xs) = if take 9 x == "departure" then y : next else next
    where next = getDepartures xs