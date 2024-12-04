module Day4 where

day4 :: IO()
day4 = do
    input <- readFile "inputs/day4.txt"
    let rows = lines input
    let answer1 = countAllChristmas rows
    print answer1

countAllChristmas :: [String] -> Int
countAllChristmas input = foldr (\x acc -> acc + findValidXMAS x input) 0 xlocations
    where
        xlocations = countX (concat input) (length (head input)) 0 0

countX :: String -> Int -> Int -> Int -> [(Int, Int)]
countX [] _ _ _= []
countX (x:xs) rowLen index rowNumb = let
    value = ([(index, rowNumb) | x == 'X'])
    (newIndex, newRow) = if index == rowLen - 1 then (0, rowNumb + 1) else (index + 1, rowNumb)
    in value ++ countX xs rowLen newIndex newRow

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
