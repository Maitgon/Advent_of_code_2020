-- Day 4.

import Data.List.Split ( splitOn )
import Data.Char ( isDigit )

main :: IO ()
main = do
    content <- readFile "input.txt"
    let data1 = map (delete "" . splitOnAnyOf ["\n", " "]) $ splitOn "\n\n" content

    -- Sol to part 1.
    let sol1' = filter isValid data1
    putStrLn $ "The solution to part 1 is: " ++ show (length sol1')

    -- Sol to part 2.
    let sol2 = length $ filter isValid2 sol1'
    putStrLn $ "The solution to part 2 is: " ++ show sol2
    


splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl (\ys d -> ys >>= splitOn d) [xs] ds

isValid :: [String] -> Bool
isValid xs = (length xs == 8) || (length xs == 7 && notElem "cid" fields)
        where fields = map (takeWhile (/= ':')) xs

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:xs)
    | x == y = xs
    | otherwise = y : delete x xs

isValid2 :: [String] -> Bool
isValid2 = all (isValid3 . span (/= ':'))

isValid3 :: (String, String) -> Bool
isValid3 ("byr", _:snum) = all isDigit snum && 1920 <= num snum && num snum <= 2002
isValid3 ("iyr", _:snum) = all isDigit snum && 2010 <= num snum && num snum <= 2020
isValid3 ("eyr", _:snum) = all isDigit snum && 2020 <= num snum && num snum <= 2030
isValid3 ("hgt", _:snum)
        | med == "cm" = 150 <= num alt && num alt <= 193
        | med == "in" = 59 <= num alt && num alt <= 76
        | otherwise = False                                        
            where (alt, med) = span isDigit snum
isValid3 ("hcl", _:hair) = head hair == '#' && foldr (\ x -> (&&) (elem x ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'])) True (tail hair)
isValid3 ("ecl", _:color) = elem color ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isValid3 ("pid", _:id) = all isDigit id && length id == 9
isValid3 _ = True

num :: String -> Int
num = read :: String -> Int



