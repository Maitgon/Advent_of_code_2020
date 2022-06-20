-- Day 11.

import Data.Array

main = do
    matrixI <- lines <$> readFile "input.txt" -- Applicative functors go brrrrrrr
    let width = length $ head matrixI
    let heigth = length matrixI
    let matrix = array ((0,0), (width-1, heigth-1)) [((i,j), (matrixI !! j) !! i) | j <- [0..heigth-1] , i <- [0..width-1]]
    
    let sol1 = sum $ map (\x -> if x == '#' then 1 else 0) $ elems $ getFixedPoint matrix
    print sol1

    let sol2 = sum $ map (\x -> if x == '#' then 1 else 0) $ elems $ getFixedPoint2 matrix
    print sol2

safeAccess :: Array (Int, Int) a -> (Int, Int) -> Maybe a
safeAccess matrix (i,j)
    | lbi <= i && i <= ubi && lbj <= j && j <= ubj = return $ matrix ! (i,j) -- You can also use pure or Just instead of return 
    | otherwise = Nothing
        where ((lbi,lbj),(ubi,ubj)) = bounds matrix


adyacentsOcuppied :: Array (Int, Int) Char -> (Int, Int) -> Int
adyacentsOcuppied matrix (i, j) = sum adyacents
    where adyacents = [ if safeAccess matrix (i-1,j-1) == Just '#' then 1 else 0
                      , if safeAccess matrix (i-1,j) == Just '#' then 1 else 0
                      , if safeAccess matrix (i,j-1) == Just '#' then 1 else 0
                      , if safeAccess matrix (i-1,j+1) == Just '#' then 1 else 0
                      , if safeAccess matrix (i+1,j-1) == Just '#' then 1 else 0
                      , if safeAccess matrix (i+1,j) == Just '#' then 1 else 0
                      , if safeAccess matrix (i,j+1) == Just '#' then 1 else 0
                      , if safeAccess matrix (i+1,j+1) == Just '#' then 1 else 0
                      ]

getCloser :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Maybe Char
getCloser matrix (i,j) (di,dj)
    | safeAccess matrix (i,j) == Just '.' = getCloser matrix (i+di,j+dj) (di,dj)
    | otherwise = safeAccess matrix (i,j)

adyacentsOcuppied2 :: Array (Int, Int) Char -> (Int, Int) -> Int
adyacentsOcuppied2 matrix (i, j) = sum adyacents
    where adyacents = [ if getCloser matrix (i-1,j-1) (-1,-1) == Just '#' then 1 else 0
                      , if getCloser matrix (i-1,j) (-1,0) == Just '#' then 1 else 0
                      , if getCloser matrix (i,j-1) (0,-1) == Just '#' then 1 else 0
                      , if getCloser matrix (i-1,j+1) (-1,1) == Just '#' then 1 else 0
                      , if getCloser matrix (i+1,j-1) (1,-1) == Just '#' then 1 else 0
                      , if getCloser matrix (i+1,j) (1,0) == Just '#' then 1 else 0
                      , if getCloser matrix (i,j+1) (0,1) == Just '#' then 1 else 0
                      , if getCloser matrix (i+1,j+1) (1,1) == Just '#' then 1 else 0
                      ]

evolveMatrix :: Array (Int, Int) Char -> Array (Int, Int) Char
evolveMatrix matrix = matrix // getChangesAux matrix 0 0 ubi ubj
    where (_,(ubi,ubj)) = bounds matrix

getChangesAux :: Array (Int, Int) Char -> Int -> Int -> Int -> Int -> [((Int, Int), Char)]
getChangesAux matrix i j ubi ubj
    | adyacentsOcuppied matrix (i, j) == 0 && matrix ! (i, j) == 'L' && i == ubi && j == ubj = [((i,j), '#')]
    | adyacentsOcuppied matrix (i, j) >= 4 && matrix ! (i, j) == '#' && i == ubi && j == ubj = [((i,j), 'L')]
    | i == ubi && j == ubj = []
    | adyacentsOcuppied matrix (i, j) == 0 && matrix ! (i, j) == 'L' && i == ubi = ((i,j), '#') : getChangesAux matrix 0 (j+1) ubi ubj
    | adyacentsOcuppied matrix (i, j) >= 4 && matrix ! (i, j) == '#' && i == ubi = ((i,j), 'L') : getChangesAux matrix 0 (j+1) ubi ubj
    | i == ubi = getChangesAux matrix 0 (j+1) ubi ubj
    | adyacentsOcuppied matrix (i, j) == 0 && matrix ! (i, j) == 'L' = ((i,j), '#') : getChangesAux matrix (i+1) j ubi ubj
    | adyacentsOcuppied matrix (i, j) >= 4 && matrix ! (i, j) == '#' = ((i,j), 'L') : getChangesAux matrix (i+1) j ubi ubj
    | otherwise = getChangesAux matrix (i+1) j ubi ubj

getFixedPoint :: Array (Int, Int) Char -> Array (Int, Int) Char
getFixedPoint matrix = if matrix == evo then matrix else getFixedPoint evo
    where evo = evolveMatrix matrix

evolveMatrix2 :: Array (Int, Int) Char -> Array (Int, Int) Char
evolveMatrix2 matrix = matrix // getChanges2Aux matrix 0 0 ubi ubj
    where (_,(ubi,ubj)) = bounds matrix

getChanges2Aux :: Array (Int, Int) Char -> Int -> Int -> Int -> Int -> [((Int, Int), Char)]
getChanges2Aux matrix i j ubi ubj
    | adyacentsOcuppied2 matrix (i, j) == 0 && matrix ! (i, j) == 'L' && i == ubi && j == ubj = [((i,j), '#')]
    | adyacentsOcuppied2 matrix (i, j) >= 5 && matrix ! (i, j) == '#' && i == ubi && j == ubj = [((i,j), 'L')]
    | i == ubi && j == ubj = []
    | adyacentsOcuppied2 matrix (i, j) == 0 && matrix ! (i, j) == 'L' && i == ubi = ((i,j), '#') : getChanges2Aux matrix 0 (j+1) ubi ubj
    | adyacentsOcuppied2 matrix (i, j) >= 5 && matrix ! (i, j) == '#' && i == ubi = ((i,j), 'L') : getChanges2Aux matrix 0 (j+1) ubi ubj
    | i == ubi = getChanges2Aux matrix 0 (j+1) ubi ubj
    | adyacentsOcuppied2 matrix (i, j) == 0 && matrix ! (i, j) == 'L' = ((i,j), '#') : getChanges2Aux matrix (i+1) j ubi ubj
    | adyacentsOcuppied2 matrix (i, j) >= 5 && matrix ! (i, j) == '#' = ((i,j), 'L') : getChanges2Aux matrix (i+1) j ubi ubj
    | otherwise = getChanges2Aux matrix (i+1) j ubi ubj

getFixedPoint2 :: Array (Int, Int) Char -> Array (Int, Int) Char
getFixedPoint2 matrix = if matrix == evo then matrix else getFixedPoint2 evo
    where evo = evolveMatrix2 matrix