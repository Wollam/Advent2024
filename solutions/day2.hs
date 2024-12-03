module Day2 where

import Data.List

day2 :: IO()
day2 = do
    input <- readFile "inputs/day2.txt"
    let rows = lines input
    let answer1 = length (filter id (map checkSafe1 rows))
    let answer2 = length (filter id (map checkSafe2 rows))
    print answer2

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
checkSafe2 input = 
    let vals = map read (words input) :: [Int]
    in checkSort vals && checkDifference vals || checkWithRemoval vals

checkWithRemoval :: [Int] -> Bool
checkWithRemoval xs = any (\i -> let xs' = removeAt i xs in checkSort xs' && checkDifference xs') [0..length xs - 1]
  where
    removeAt i xs = take i xs ++ drop (i + 1) xs

