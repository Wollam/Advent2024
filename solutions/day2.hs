module Day2 where

import Data.List
import Debug.Trace
day2 :: IO()
day2 = do
    input <- readFile "inputs/day2.txt"
    let rows = lines input
    let answer1 = length (filter id (map checkSafe1 rows))
    let answer2 = length (filter id (map checkSafe1 rows))
    print answer1

checkSafe1 :: String -> Bool
checkSafe1 input = let
    vals = map read (words input) :: [Int]
    in checkSort vals && checkDifference vals

checkSort :: [Int] -> Bool
checkSort xs = isAscending xs || isDescending xs

isAscending :: [Int] -> Bool
isAscending (x:xs:ys) = x <= xs && isAscending (xs:ys) 
isAscending _ = True

isDescending :: [Int] -> Bool
isDescending (x:xs:ys) = x >= xs && isDescending (xs:ys) 
isDescending _ = True

checkDifference :: [Int] -> Bool
checkDifference (x:xs:ys) = (diff >= 1 && diff <= 3) && checkDifference (xs:ys)
    where
        diff = abs (xs - x)
checkDifference _ = True

checkSafe2 :: String -> Bool
checkSafe2 input = let
    vals = map read (words input) :: [Int]
    in True
