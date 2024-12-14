{-# LANGUAGE ScopedTypeVariables #-}
module Day5 where

import Data.List
import Data.Map as Map (empty, Map, fromListWith, member, (!), )
import Data.IntSet as Set (IntSet, insert, union, singleton, member)
import Data.Maybe

day5 :: IO()
day5 = do
    input <- readFile "inputs/day5.txt"
    let (rules, updates) = processInput5 input
    let answer1 = sum (mapMaybe (validUpdateNumber rules) updates)

    print answer1

processInput5 :: String -> (Map Int IntSet, [[Int]])
processInput5 input = let
    rows = lines input
    rulesString = map (wordsWhen (=='|')) (takeWhile (not . null) rows)
    rulesList :: [(Int, Int)] = map (\(x:xs:_) -> (read x, read xs)) rulesString
    ruleDict = Map.fromListWith Set.union [(k, Set.singleton v) | (k, v) <- rulesList]

    updateString = map (wordsWhen (==',')) (tail (dropWhile (not . null) rows))
    updates :: [[Int]] = map (map read) updateString
    in (ruleDict, updates)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

validUpdateNumber :: Map Int IntSet -> [Int] -> Maybe Int
validUpdateNumber rules xs = if validUpdateCheck rules xs then Just (xs !! (length xs `div` 2 )) else Nothing

validUpdateCheck :: Map Int IntSet -> [Int] -> Bool
validUpdateCheck rules xs = checkReverse rules (reverse xs)
    where
        checkReverse :: Map Int IntSet -> [Int] -> Bool
        checkReverse rules [] = True
        checkReverse rules (x:xs) = not (any (`Set.member` ruleX) xs) && checkReverse rules xs
            where
                ruleX = rules ! x