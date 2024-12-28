{-# LANGUAGE ScopedTypeVariables #-}
module Day7 where

import Data.List
import Debug.Trace

day7 :: IO()
day7 = do
    input <- readFile "inputs/day7.txt"
    let rows = lines input
    let (sumStrings, numberStrings) = unzip (map (span (/=':')) rows)
    let sums :: [Int] = map read sumStrings
    let numbers :: [[Int]] = map (map read . words . tail) numberStrings
    let sumsNums = zip sums numbers
    let answer1 = part1 sumsNums
    print answer1

part1 :: [(Int,[Int])] -> Int
part1 input = let
    totals =  map countConfigs input
    in sum totals

countConfigs :: (Int,[Int]) -> Int
countConfigs (total, vals) = let
    possibles = allPossibleCombos (reverse vals)
    matches = filter (==total) possibles
    in if not (null matches) then total else 0

allPossibleCombos :: [Int] -> [Int]
allPossibleCombos [x] = [x]
allPossibleCombos (x:xs)  = 
    [x + y | y <- allPossibleCombos xs] ++ 
    [x * y | y <- allPossibleCombos xs] ++ 
    [x @|| y | y <- allPossibleCombos xs]

(@||) :: Int -> Int -> Int
x @|| y = x * 10 ^ numDigits y + y

-- Helper function to calculate the number of digits in an integer
numDigits :: Int -> Int
numDigits n
  | n < 10    = 1
  | otherwise = 1 + numDigits (n `div` 10)