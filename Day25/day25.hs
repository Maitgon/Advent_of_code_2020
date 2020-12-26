-- Day 23.

main :: IO ()
main = do
    -- Input reading.
    [doorpk, cardpk] <- map (read :: String -> Int) . lines <$> readFile "input.txt"
    print doorpk

    -- Sol to part 1.
    let cardLoop = getLoopVal cardpk
    let doorLoop = getLoopVal doorpk
    print (cardLoop, doorLoop)
    let key = transform cardLoop doorpk
    print key

subNum :: Int
subNum = 7

modVal :: Int
modVal = 20201227

getLoopVal :: Int -> Int
getLoopVal pub = getLoopValAux pub 0 1

getLoopValAux :: Int -> Int -> Int -> Int
getLoopValAux pub loop cur
    | cur == pub = loop
    | otherwise = getLoopValAux pub (loop+1) (mod (cur*subNum) modVal)

transform :: Int -> Int -> Int
transform loop sub = transformAux loop sub 1

transformAux :: Int -> Int -> Int -> Int
transformAux 0 sub subK = subK
transformAux loop sub subK = transformAux (loop-1) sub (mod (subK*sub) modVal)