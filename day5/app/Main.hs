module Main where

import Lib
import Data.List.Split
import qualified Data.Map as Map

main :: IO ()
main = do
    inp <- lines <$> readFile "day5.txt"
    print $ greaterThan2 $ createMap $ concat $ filter p2 (map parseInput inp)

newtype Point = Point (Int, Int)
  deriving Show

instance Eq Point where
    (Point (x1,y1)) == (Point (x2,y2)) = x1 == x2 && y1 == y2

instance Ord Point where
    (Point (x1,y1)) <= (Point (x2,y2)) | x1 == x2 = y1 < y2
                                       | otherwise = x1 < x2

p1 :: [Point] -> Bool
p1 ((Point (x1,y1)):(Point (x2,y2)):_) = x1 == x2 || y1 == y2
p1 _ = True

p2 :: [Point] -> Bool
p2 _ = True

parseInput :: String -> [Point]
parseInput ls = genPoints x1 x2 y1 y2
                where w = words ls
                      [x1,y1] = map read (splitOn "," (head w))
                      [x2,y2] = map read (splitOn "," (last w))

genPoints :: Int -> Int -> Int -> Int -> [Point]
genPoints x1 x2 y1 y2 | abs (x1 - x2) > abs (y1 - y2) = zipWith makePoint xs (cycle  ys)
                      | otherwise = zipWith makePoint (cycle xs) ys
                        where
                          xs = genList x1 x2
                          ys = genList y1 y2
                          makePoint a b = Point (a,b)

genList :: Int -> Int -> [Int]
genList z z' | z > z' = enumFromThenTo z (z - 1) z'
             | otherwise = enumFromTo z z'

createMap :: [Point] -> Map.Map Point Int
createMap = foldl insertPointToMap Map.empty

insertPointToMap :: Map.Map Point Int -> Point -> Map.Map Point Int
insertPointToMap m point = Map.insertWith (+) point 1 m

greaterThan2 :: Map.Map Point Int -> Int
greaterThan2 = foldr (\x -> if x > 1 then (1+) else (0+)) 0
