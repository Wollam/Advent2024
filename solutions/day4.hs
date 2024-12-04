module Day4 where

import Debug.Trace

day4 :: IO()
day4 = do
    input <- readFile "inputs/day4.txt"
    let rows = lines input
    let answer1 = countAllChristmas rows
    let answer2 = countAllChristmas2 rows
    print answer2

countAllChristmas :: [String] -> Int
countAllChristmas input = foldr (\x acc -> acc + findValidXMAS x input) 0 xlocations
    where
        xlocations = findChar (concat input) (length (head input)) 0 0 'X'

findChar :: String -> Int -> Int -> Int -> Char -> [(Int, Int)]
findChar [] _ _ _ _= []
findChar (x:xs) rowLen index rowNumb charact = let
    value = ([(index, rowNumb) | x == charact])
    (newIndex, newRow) = if index == rowLen - 1 then (0, rowNumb + 1) else (index + 1, rowNumb)
    in value ++ findChar xs rowLen newIndex newRow charact

findValidXMAS :: (Int, Int) -> [String] -> Int
findValidXMAS (x, y) matrix = let
    boundx = length (head matrix)
    boundy = length matrix
    dirs = [(1,0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1), (1,1)]
    checkLocs = [ [(x + a, y + b), (x + 2*a, y + 2*b), (x + 3*a, y + 3*b)] | (a,b) <- dirs,
        x + 3*a >= 0 && x + 3*a < boundx && y + 3*b >= 0 && y +3*b < boundy]
    valids = map (validateXMAS matrix) checkLocs
    in length (filter id valids)

validateXMAS ::  [String] -> [(Int, Int)] -> Bool
validateXMAS matrix ((x1, y1):(x2,y2):(x3,y3):_) =
    (matrix !! y1) !! x1 == 'M' &&
    (matrix !! y2) !! x2 == 'A' &&
    (matrix !! y3) !! x3 == 'S'

countAllChristmas2 :: [String] -> Int
countAllChristmas2 input = length (filter id valids)
    where
        valids = map (`findValidXMAS2` input) xlocations
        xlocations = findChar (concat input) (length (head input)) 0 0 'A'

findValidXMAS2 :: (Int, Int) -> [String] -> Bool
findValidXMAS2 (x, y) matrix = let
    boundx = length (head matrix)
    boundy = length matrix
    dirs = [(-1 , 1), (1, 1), (1, -1), (-1,-1)]
    checkLocs =
        [ (x + a, y + b) | (a, b) <- dirs
        , let newX = x + a, let newY = y + b
        , newX >= 0 && newX < boundx && newY >= 0 && newY < boundy
        ]
    chars = [(matrix !! yloc) !! xloc | (xloc, yloc) <- checkLocs]
    in trace chars countChar 'M' chars == 2 && countChar 'S' chars == 2 && chars /= "MSMS" && chars /= "SMSM"

countChar :: Char -> String -> Int
countChar _ [] = 0
countChar c (x:xs)
    | c == x    = 1 + countChar c xs
    | otherwise = countChar c xs




