module Day6 where

import Data.List
import Data.Set as Set ( Set, insert,size, empty, member )
import Debug.Trace
day6 :: IO()
day6 = do
    input <- readFile "inputs/day6.txt"
    let rows = lines input 
    let Just (startY, startX) = findGuard rows
    let answer1 = Set.size (uniqueSteps (startX, startY) (0,-1) rows Set.empty)
    print answer1

findGuard :: [String] -> Maybe(Int, Int)
findGuard rows = do
    let flatRows = concat rows
    let lenRow = length (head rows)
    index <- '^' `elemIndex` flatRows
    return (index `div` lenRow, index `mod` lenRow)

uniqueSteps :: (Int, Int) -> (Int, Int) -> [String]  -> Set (Int, Int) -> Set (Int, Int)
uniqueSteps (startX, startY) (dirX, dirY) rows steps = let
    limitX = length (head rows) - 1
    limitY = length rows - 1
    newSteps = Set.insert (startX, startY) steps
    (newX, newY) = (startX + dirX, startY + dirY)
    in if newX < 0 || newX > limitX || newY < 0 || newY > limitY then newSteps
        else case (rows !! newY) !! newX of
            '#' -> uniqueSteps (startX, startY) (-dirY, dirX) rows newSteps
            _ -> uniqueSteps (newX, newY) (dirX, dirY) rows newSteps
            
            
            
    

    