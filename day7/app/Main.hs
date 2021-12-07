module Main where

import Lib
import Data.List.Split
import Data.List
import Debug.Trace

main :: IO ()
main = do
    inpTest <- readFile "day7Test.txt"
    inp <- readFile "day7.txt"
    print $ findMostEfficientPoint  $ map read (splitOn "," inpTest) -- 37
    print $ findMostEfficientPoint  $ map read (splitOn "," inp) -- 349357
    print $ findMostEfficientPoint2 $ map read (splitOn "," inpTest) -- 168
    print $ findMostEfficientPoint2 $ map read (splitOn "," inp) -- 96708205

type Position = Int

findMostEfficientPoint :: [Position] -> Int
findMostEfficientPoint pos = fuelToPoint pos $ medianInt pos

medianInt :: [Int] -> Either Int (Int, Int)
medianInt l | odd len = Right (avgC, avgC + 1)
            | even len = Left $ sortedL !! mid
          where 
             sortedL = sort l
             len = length sortedL
             mid = len `div` 2
             c1 = sortedL !! mid
             c2 = sortedL !! (mid + 1)
             avgC = (c1 + c2) `div` 2

findMostEfficientPoint2 :: [Position] -> Int
findMostEfficientPoint2 pos = min (sum $ map (cost avg) pos) (sum $ map (cost (avg + 1)) pos)
                             where
                                 avg = sum pos `quot` length pos
                                 cost av x = let d = abs (av - x) in d * (d + 1) `quot` 2

fuelToPoint :: [Position] -> Either Int (Int, Int) -> Int
fuelToPoint pos (Left p) = sum $ map (\x -> abs $ p - x) pos
fuelToPoint pos (Right (p1, p2)) = min p1Sum p2Sum
                                  where
                                      p1Sum = sum $ map (\x -> abs $ p1 - x) pos
                                      p2Sum = sum $ map (\x -> abs $ p2 - x) pos
