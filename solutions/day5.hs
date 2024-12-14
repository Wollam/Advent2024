{-# LANGUAGE ScopedTypeVariables #-}
module Day5 where

import Data.List
import Data.Map as Map (empty, Map, fromListWith)
import Data.IntSet as Set (IntSet, insert, union, singleton)

day5 :: IO()
day5 = do
    input <- readFile "inputs/day5.txt"
    let rows = lines input
    let rulesString = map (wordsWhen (=='|')) (takeWhile (not . null) rows)
    let rulesList :: [(Int, Int)] = map (\(x:xs:_) -> (read x, read xs)) rulesString
    let ruleDict = Map.fromListWith Set.union [(k, Set.singleton v) | (k, v) <- rulesList]

    let updateString = map (wordsWhen (==',')) (tail (dropWhile (not . null) rows))
    let updates :: [[Int]] = map (map read) updateString
    print ruleDict
    return()

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'