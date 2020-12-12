-- Day 12.

type Instruction = (Char, Int)
--data Direction = West | North | East | South
                -- 270    180     90     0
    --deriving (Eq, Show, Read)
type Direction = Int
type Position = (Int, Int)
type Boat = (Position, Direction)
type Boat2 = (Position, Position)

main :: IO ()
main = do
    -- Input reading.
    instructions <- map (\(x:xs) -> (x, read xs :: Int)) . lines <$> readFile "input.txt"

    -- Part 1.
    let sol1 = manhattanDistance $ fst $ foldl doInstruction ((0,0), 90) instructions
    putStrLn $ "The solution to part 1 is: " ++ show sol1

    -- Part 2.
    let sol2 = manhattanDistance $ fst $ foldl doInstruction2 ((0,0), (10,1)) instructions
    putStrLn $ "The solution to part 2 is: " ++ show sol2


doInstruction :: Boat -> Instruction -> Boat
doInstruction ((x,y), d) ('N', move) = ((x,y+move), d)
doInstruction ((x,y), d) ('S', move) = ((x,y-move), d)
doInstruction ((x,y), d) ('E', move) = ((x+move,y), d)
doInstruction ((x,y), d) ('W', move) = ((x-move,y), d)
doInstruction ((x,y), d) ('R', degree) = ((x,y), mod (d - degree) 360)
doInstruction ((x,y), d) ('L', degree) = ((x,y), mod (d + degree) 360)
doInstruction ((x,y), d) ('F', move)
    | d == 0 = ((x,y-move), d)
    | d == 90 = ((x+move,y), d)
    | d == 180 = ((x,y+move), d)
    | d == 270 = ((x-move,y), d)

manhattanDistance :: Position -> Int
manhattanDistance (x,y) = abs x + abs y

rotation :: Position -> Int -> Position
rotation (x,y) degrees = (round (rotc*xf - rots*yf), round (rots*xf + rotc*yf))
    where rotc = cos (2*pi* fromIntegral degrees /360.0)
          rots = sin (2*pi* fromIntegral degrees /360.0)
          xf = fromIntegral x
          yf = fromIntegral y

doInstruction2 :: Boat2 -> Instruction -> Boat2
doInstruction2 ((x,y),(wx, wy)) ('N', move) = ((x,y),(wx,wy+move))
doInstruction2 ((x,y),(wx, wy)) ('S', move) = ((x,y),(wx,wy-move))
doInstruction2 ((x,y),(wx, wy)) ('E', move) = ((x,y),(wx+move,wy))
doInstruction2 ((x,y),(wx, wy)) ('W', move) = ((x,y),(wx-move,wy))
doInstruction2 ((x,y),(wx, wy)) ('L', degree) = ((x,y), rotation (wx,wy) degree)
doInstruction2 ((x,y),(wx, wy)) ('R', degree) = ((x,y), rotation (wx,wy) (-degree))
doInstruction2 ((x,y),(wx, wy)) ('F', move) = ((x + wx*move, y + wy*move), (wx,wy))


