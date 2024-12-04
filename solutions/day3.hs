module Day3 where

import Text.Regex.Base
import Text.Regex.Posix

day3 :: IO ()
day3 = do
    input <- readFile "inputs/day3.txt"
    let answer1 = calculateTotal (matchRegex input)
    let answer2 = calculateTotal2 (matchRegex2 input) True
    print answer2

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

matchRegex2 :: String -> [String]
matchRegex2 input = getAllTextMatches (input =~ regex)
    where
        regex = "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)"

calculateTotal2 :: [String] -> Bool -> Int
calculateTotal2 [] _ = 0
calculateTotal2 (x:xs) enabled = case x of
    "do()" -> calculateTotal2 xs True
    "don't()" -> calculateTotal2 xs False
    _ -> if enabled then l * r + calculateTotal2 xs enabled else calculateTotal2 xs enabled
        where
            (l, r) = extractIntegers x
