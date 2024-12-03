
module Day3 where

import Text.Regex.Base
import Text.Regex.Posix
import Data.Char (isDigit, isAlpha)

day3 :: IO ()
day3 = do
    input <- readFile "inputs/day3.txt"
    let answer1 = calculateTotal (matchRegex input)
    print answer1

matchRegex :: String -> [String]
matchRegex input = getAllTextMatches (input =~ regex)
    where
        regex = "mul\\([0-9]+,[0-9]+\\)"

calculateTotal :: [String] -> Int
calculateTotal = foldr f 0 
    where
        f x acc = acc + l * r
            where
                (l, r) = extractIntegers x

extractIntegers :: String -> (Int, Int)
extractIntegers input =
    case break (== '(') input of
        (_, '(' : rest) -> case break (== ')') rest of
            (inside, _) ->
                let (a, b) = break (== ',') inside
                in (read a, read (tail b))
        _ -> error "No opening parenthesis found"
