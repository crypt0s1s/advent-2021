module Main where

import Lib

main :: IO Int
main = do
    inp <- lines <$> readFile "day1.txt"
    let measurements = map read inp
    return $ p2 measurements

p1 :: [Int] -> Int
p1 nums = zipWithSum (drop 1 nums) nums

p2 :: [Int] -> Int
p2 nums = p1 $ zipWith3 (\a b c -> a + b + c) (drop 2 nums) (drop 1 nums) nums

zipWithSum :: [Int] -> [Int] -> Int
zipWithSum []     _bs    = 0
zipWithSum _as    []     = 0
zipWithSum (a:as) (b:bs) | b < a     = 1 + zipWithSum as bs
                         | otherwise = zipWithSum as bs