module Day8 where

import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

day8 :: IO()
day8 = do
    input <- readFile "inputs/day8.txt"
    let rows = lines input
    let answer1 = part1 rows
    print answer1

part1 :: [String] -> Int
part1 rows = let
    locs = findLocs rows
    combs = (concat . M.elems .M.map uniqueCombs) locs
    limitY = length rows
    limitX = length (head rows)
    allAntis = S.unions (map findAntiNode combs)
    filtedAntis = S.filter (\(x,y) -> x >= 0 && x < limitX && y >= 0 && y < limitY) allAntis
    in length filtedAntis

findLocs :: [String] -> M.Map Char [(Int,Int)]
findLocs rows = 
  let lenX = length (head rows)
      (_, result) = foldl f (0, M.empty) (concat rows)
      f (enumAcc, resultAcc) x =
        let key = x
            value = (enumAcc `mod` lenX , enumAcc `div` lenX)
        in if x /= '.' then (enumAcc + 1, M.insertWith (++) key [value] resultAcc) else (enumAcc + 1, resultAcc)
  in result

uniqueCombs:: [a] -> [(a, a)]
uniqueCombs [] = []
uniqueCombs (x:xs) = [(x, y) | y <- xs] ++ uniqueCombs xs

findAntiNode :: ((Int,Int),(Int,Int)) -> S.Set (Int,Int)
findAntiNode ((x1,y1),(x2,y2)) = let
    dirY = y2 - y1
    dirX = x2 - x1
    antiNode1 = (x1 - dirX, y1 - dirY)
    antiNode2 = (x2 + dirX, y2 + dirY)
    in S.fromList [antiNode1,antiNode2]