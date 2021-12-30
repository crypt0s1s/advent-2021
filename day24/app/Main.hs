-- credit - https://github.com/kemmel-dev/AdventOfCode2021/blob/master/day24/AoC%20Day%2024.pdf
module Main where

import Lib
import Data.Char (digitToInt, isAlpha)
import Data.List.Split
import Debug.Trace (traceShowId, traceShow)

type Check = Int
type Offset = Int
type Stack = [(Var,Offset)]
type Var = Int
type ModelNum = Int

main :: IO ()
main = do
         inp <- lines <$> readFile "day24.txt"
         let parsed = parseInput inp
         print $ p1 parsed
         print $ p2 parsed

p1 :: [(Check,Offset)] -> ModelNum
p1 checksOffsets = snd $ foldl findMaxNum ([],0) $ zip [13,12..0] checksOffsets

p2 :: [(Check,Offset)] -> ModelNum
p2 checksOffsets = snd $ foldl findMinNum ([],0) $ zip [13,12..0] checksOffsets

parseInput :: [String] -> [(Check, Offset)]
parseInput ls = map getCheckOffset chunks
                where
                    chunks = chunksOf 18 ls
                    getCheckOffset :: [String] -> (Check, Offset)
                    getCheckOffset chunk = (read (wCheck !! 2), read (wOffset !! 2))
                                           where
                                               wCheck = words (chunk !! 5)
                                               wOffset = words (chunk !! 15)

findMaxNum :: (Stack,ModelNum) -> (Var,(Check, Offset)) -> (Stack,ModelNum)
findMaxNum (stack,num) (var,(check,offset)) | check > 0 = ((var,offset):stack,num)
                                            | otherwise = (stack',num')
                                              where
                                                  (b,a) | o + check < 0 = (9, 9 + (o + check))
                                                        | otherwise = (9 - (o + check),9)
                                                  num' = num + (a * (10 ^ var)) + (b * (10 ^ v))
                                                  (v,o) = head stack
                                                  stack' = tail stack

findMinNum :: (Stack,ModelNum) -> (Var,(Check, Offset)) -> (Stack,ModelNum)
findMinNum (stack,num) (var,(check,offset)) | check > 0 = ((var,offset):stack,num)
                                            | otherwise = (stack',num')
                                              where
                                                  (b,a) | o + check < 0 = (1 - (o + check),1)
                                                        | otherwise = (1,1 + (o + check))
                                                  num' = num + (a * (10 ^ var)) + (b * (10 ^ v))
                                                  (v,o) = head stack
                                                  stack' = tail stack
