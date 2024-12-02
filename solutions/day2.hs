{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Day2 where

import Data.List
import Debug.Trace
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
checkSafe2 input = let
    vals = map read (words input) :: [Int]
    in checkSequence vals

checkSequence :: [Int] -> Bool
checkSequence xs = let
    ascend = oddAscending xs
    descend = oddDescending xs
    difference = oddDifference xs
    sortList = if length ascend > length descend then descend else ascend
    in checkLists xs sortList difference

checkLists :: [Int] -> [Int] -> [Int] -> Bool
checkLists xs sortList difference
  | null sortList && null difference = True
  | length sortList > 1 || length difference > 1 = False
  | not (null sortList || null difference) = sortList == difference
  | null sortList = checkNewList xs difference
  | otherwise = checkNewList xs sortList

checkNewList :: [Int] -> [Int] -> Bool
checkNewList xs removevals = checkSort checkList && checkDifference checkList
    where
        checkList =  xs \\ removevals

oddAscending :: [Int] -> [Int]
oddAscending (x:xs:ys) = if x <= xs
    then oddAscending (xs:ys)
    else xs : oddAscending (xs:ys)
oddAscending _ = []

oddDescending :: [Int] -> [Int]
oddDescending (x:xs:ys) = if x >= xs
    then oddDescending (xs:ys)
    else xs : oddDescending (xs:ys)
oddDescending _ = []

oddDifference :: [Int] -> [Int]
oddDifference (x:xs:ys) = if diff >= 1 && diff <= 3
    then oddDifference (xs:ys)
    else x : oddDifference (xs:ys)
    where
        diff = abs (xs - x)
oddDifference _ = []

test2 = do
    let example = [31, 32, 30, 33, 34, 37] 
    print (oddAscending example)
    print (oddDescending example)
   