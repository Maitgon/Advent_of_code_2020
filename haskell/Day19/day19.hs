-- Day 19.

import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified Text.Regex as R

--type Parser = Parsec String ()
data Regex = Concat [Regex] | Choice Regex Regex | Subs Regex | Var Char
    deriving (Eq)


instance Show Regex where
    show (Concat as) = foldl1 (++) (map show as)
    show (Choice a b) = '(' : show a ++ "|" ++ show b ++ ")"
    show (Subs a) = show a
    show (Var c) = [c]


main :: IO ()
main = do
    -- input reading.
    [input1, input2] <- map lines . splitOn "\n\n" . filter (/= '"') <$> readFile "input.txt"
    let regex = M.fromList $ map ((\(x,y:xs) -> (x, (filter (/= "") . words) xs)) . span (/= ':')) input1
    [input3, input4] <- map lines . splitOn "\n\n" . filter (/= '"') <$> readFile "input2.txt"
    let regex2 = M.fromList $ map ((\(x,y:xs) -> (x, (filter (/= "") . words) xs)) . span (/= ':')) input3
    -- print regex

    -- Part 1.
    let deb1 = toRegex regex
    let deb2 = R.mkRegexWithOpts ('^' : show deb1 ++ "$") True False
    print $ sum $ map ((\x -> if isJust x then 1 else 0) . R.matchRegex deb2) input2

    -- Part 2.
    let deb3 = toRegex regex2
    let deb4 = R.mkRegexWithOpts ('^' : show deb3 ++ "$") True False
    print $ sum $ map ((\x -> if isJust x then 1 else 0) . R.matchRegex deb4) input4


splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl (\ys d -> ys >>= splitOn d) [xs] ds

toRegex :: M.Map String [String] -> Regex
toRegex nRegex = toRegexAux nRegex "0"

toRegexAux :: M.Map String [String] -> String -> Regex
toRegexAux nRegex exp
    | "|" `elem` res = Choice
                       (Concat (map (toRegexAux nRegex) iz))
                       (Concat (map (toRegexAux nRegex) dr))
    | length res == 1 = if res == ["a"] || res == ["b"]
                        then Var ((head . head) res)
                        else Subs (toRegexAux nRegex $ head res)
    | otherwise = Concat $ map (toRegexAux nRegex) res

        where res = nRegex M.! exp
              (iz, _:dr) = span (/= "|") res


{-
toRegexAux :: M.Map String [String] -> String -> Regex
toRegexAux nRegex exp
    | length res == 4 = Choice
                        (Concat (toRegexAux nRegex $ head res) (toRegexAux nRegex $ res !! 1))
                        (Concat (toRegexAux nRegex $ res !! 2) (toRegexAux nRegex $ res !! 3))
    | length res == 2 = Concat (toRegexAux nRegex $ head res) (toRegexAux nRegex $ last res)
    | length res == 3 = Concat (Concat (toRegexAux nRegex $ head res) (toRegexAux nRegex $ res !! 1)) (toRegexAux nRegex $ last res)
    | head res == "a" || head res == "b" = Var $ (head . head) res
    | otherwise = Subs (toRegexAux nRegex $ head res)
        where res = nRegex M.! exp
-}