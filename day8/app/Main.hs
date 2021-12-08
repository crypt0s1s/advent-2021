module Main where

import Lib
import Data.List.Split
import Debug.Trace

main :: IO ()
main = do
    inpTest <- lines <$> readFile "day8Test.txt"
    inp <- lines <$> readFile "day8.txt"
    print $ count1478s $ map (snd . processLine) inpTest
    print $ count1478s $ map (snd . processLine) inp
    print $ sum $ decode $ map processLine inpTest
    print $ sum $ decode $ map processLine inp

processLine :: String -> ([String],[String])
processLine xs = (inp1,inp2)
                where
                    [sp1,sp2] = splitOn "|" xs
                    inp1 = words sp1
                    inp2 = words sp2

count1478s :: [[String]] -> Int
count1478s ls = sum $ map (\x -> if length x <= 4 || length x == 7 then 1 else 0) singleL
                where singleL = concat ls

decode :: [([String],[String])] -> [Int]
decode = map (read . decode')

decode' :: ([String],[String]) -> String
decode' (a,b) = map (`findNumber` a) b

findNumber :: String -> [String] -> Char
findNumber s ls | len == 2  = '1'
                | len == 3  = '7'
                | len == 4  = '4'
                | len == 5  = find5 s n1 n4
                | len == 6  = find6 s n4 n7
                | len == 7  = '8'
                | otherwise = error "Not a valid lenght"
                where
                    len = length s
                    n1  = findElemOfLenN ls 2
                    n7  = findElemOfLenN ls 3
                    n4  = findElemOfLenN ls 4

findElemOfLenN :: [String] -> Int -> String
findElemOfLenN xs n = head $ filter (\x -> n == length x) xs

-- the string of length 5 in question and the 1 encoding
find5 :: String -> String -> String -> Char
find5 x n1 n4 | all (`elem` x) n1 = '3'
              | length (filter (`elem` x) n4) == 3 = '5'
              | otherwise = '2'

-- the string of length 6 in question, the 4 decoding and the 7 decoding
find6 :: String -> String -> String -> Char
find6 x n4 n7 | all (`elem` x) n4 = '9'
              | all (`elem` x) n7 = '0'
              | otherwise = '6'
