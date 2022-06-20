-- Day 14

import Data.Char ( digitToInt, intToDigit )
import qualified Data.Map as M

data Instruction = Mask | Mem Int
  deriving(Show, Read, Eq)

type Memory = M.Map Int Int

main = do
  -- Input reading.
  input <- map (parseIn . words) . lines <$> readFile "input.txt"

  -- Sol to part 1.
  let sol1 = sum $ M.elems $ applyRules input [] M.empty
  print sol1

  -- Sol to part 2.
  let sol2 = sum $ M.elems $ applyRules2 input [] M.empty
  print sol2


parseIn :: [String] -> (Instruction, String)
parseIn [inst,_,num]
  | inst == "mask" = (Mask, num)
  | otherwise = (Mem (read (drop 4 (init inst)) :: Int), (fill36 . toBin) (read num :: Int))

fill36 :: String -> String
fill36 xs = replicate (36 - length xs) '0' ++ xs

toBin 0 = "0"
toBin n = reverse (helper n)

helper 0 = []
helper n = intToDigit (n `mod` 2) : helper (n `div` 2)

fromBin [] = 0
fromBin xs = helper' xs (length xs -1)

helper' [] _ = 0
helper' [x] _ = digitToInt x
helper' (x:xs) n = (2*digitToInt x)^n + helper' xs (n-1)

applyRules :: [(Instruction, String)] -> String -> Memory -> Memory
applyRules [] _ mem = mem
applyRules ((Mask, mask):ins) _ mem = applyRules ins mask mem
applyRules ((Mem pos, value):ins) mask mem = applyRules ins mask (M.insert pos (combine mask value) mem)

combine :: String -> String -> Int
combine mask value = fromBin $ zipWith combine' mask value

combine' :: Char -> Char -> Char
combine' '1' _ = '1'
combine' '0' _ = '0'
combine' 'X' c = c

applyRules2 :: [(Instruction, String)] -> String -> Memory -> Memory
applyRules2 [] _ mem = mem
applyRules2 ((Mask, mask):ins) _ mem = applyRules2 ins mask mem
applyRules2 ((Mem pos, value):ins) mask mem = applyRules2 ins mask (M.union new mem)
    where new = M.fromList $ zip memS $ repeat $ fromBin value
          memS = (map fromBin . getAll) (combine2 mask $ (fill36 . toBin) pos)

combine2 :: String -> String -> String
combine2 mask value = zipWith combine2' mask value

combine2' :: Char -> Char -> Char
combine2' '0' c = c
combine2' '1' _ = '1'
combine2' 'X' _ = 'X'

getAll :: String -> [String]
getAll xs = if 'X' `elem` xs then getAll (replace xs 'X' '0') ++ 
                                  getAll (replace xs 'X' '1')
                             else [xs]

replace :: Eq a => [a] -> a -> a -> [a]
replace [] _ _ = []
replace (x:xs) y z = if x == y then z:xs else x : replace xs y z