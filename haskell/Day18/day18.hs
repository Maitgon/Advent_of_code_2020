-- Day 18.

import qualified Data.List as L
import Data.Char ( digitToInt, isDigit )

data Token = Sum | Mult | None
    deriving (Eq, Show)
data Expr = D Token Expr Expr | Par Expr | Value Int
    deriving (Eq, Show)

main :: IO ()
main = do
    -- Input reading.
    input <- lines . filter (/=' ') <$> readFile "input.txt"

    -- Sol to part 1.
    let sol1 = sum $ map (semantics . parse) input
    putStrLn $ "The solution to part 1 is: " ++ show sol1

    -- Sol to part 2.
    let sol2 = sum $ map (semantics . parse . ('(' :) . readForPart2) input
    putStrLn $ "The solution to part 2 is: " ++ show sol2

readForPart2 :: String -> String
readForPart2 [] = [')']
readForPart2 (x:xs) 
    | x == '*' = ")*(" ++ readForPart2 xs 
    | x == '(' = "((" ++ readForPart2 xs
    | x == ')' = "))" ++ readForPart2 xs
    | otherwise = x:readForPart2 xs

parse :: String -> Expr
parse xs = parseAux xs (Value 0) None

parseAux :: String -> Expr -> Token -> Expr
parseAux [] exp _ = exp
parseAux (x:xs) exp nextT
    | isDigit x = if nextT == None 
                  then parseAux xs (Value (digitToInt x)) None 
                  else parseAux xs (D nextT exp (Value (digitToInt x))) None
    | x == '+' = parseAux xs exp Sum
    | x == '*' = parseAux xs exp Mult
    | x == '(' = if nextT == None
                 then parseAux rightExp (D Sum exp (Par (parseAux leftExp (Value 0) None))) None
                 else parseAux rightExp (D nextT exp (Par (parseAux leftExp (Value 0) None))) None
        where (leftExp, _:rightExp) = splitAt (getRightPar xs 0 0) xs

getRightPar :: String -> Int -> Int -> Int
getRightPar [] _ n = n
getRightPar (x:xs) m n
    | m == 0 && x == ')' = n
    | x == ')' = getRightPar xs (m-1) (n+1)
    | x == '(' = getRightPar xs (m+1) (n+1)
    | otherwise = getRightPar xs m (n+1)

semantics :: Expr -> Int
semantics (Value x) = x
semantics (D Sum exp1 exp2) = semantics exp1 + semantics exp2
semantics (D Mult exp1 exp2) = semantics exp1 * semantics exp2
semantics (Par x) = semantics x



