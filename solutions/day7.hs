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
allPossibleCombos (x:xs)  = [x + y | y <- allPossibleCombos xs] ++ [x * y | y <- allPossibleCombos xs]

