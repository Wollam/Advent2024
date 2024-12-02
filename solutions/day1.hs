module Day1 where

import Data.List (foldr, sort)
import Data.Map as Map (Map, empty, insertWith, lookup) 

day1 :: IO ()
day1 = do
    input <- readFile "inputs/day1.txt"
    let rows = lines input
    let answer1 = part1 rows
    let answer2 = part2 rows
    print answer2

part1 :: [String] -> Int
part1 input = let 
    (listL, listR) = splitInput input
    (sortL, sortR) = (sort listL, sort listR)
    in countTogether (zip sortL sortR)

splitInput :: [String] -> ([String], [String])
splitInput = foldr f e 
    where 
        e = ([],[])
        f x (list1, list2) = (x1: list1, x2:list2)
            where 
                (x1: x2: _) = words x

countTogether :: [(String, String)] -> Int
countTogether = foldr f 0 
    where
        f (val1, val2) acc = acc + abs (read val2 - read val1)

part2 :: [String] -> Int
part2 input = let
    (listL, listR) = splitInput input
    rightOccurences = countOccurences listR
    in compareLists listL rightOccurences

countOccurences :: [String] -> Map Int Int 
countOccurences = foldr f e
    where
        f x = insertWith (+) (read x) 1 
        e = empty

compareLists :: [String] -> Map Int Int -> Int
compareLists l m = foldr f 0 l
    where
        f x acc = case Map.lookup (read x) m of
            Nothing -> acc 
            Just val -> acc + val * read x