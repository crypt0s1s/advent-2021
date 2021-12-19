{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Lib
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
type StepValVel = Map.Map Int [Int]

main :: IO ()
main = do
    print $ p1 $ snd target'
    print $ p2 target'

p1 :: (Int,Int) -> Int
p1 (y1,y2) = findMaxYPos $ findMaxYVel (y1,y2)

p2 :: ((Int,Int),(Int,Int)) -> Int
p2 t = length $ Set.toList . Set.fromList $ concatMap snd $ Map.toList intersectionMap
     where
         (x,y) = t
         valYSteps = findValidYVelsWSteps y
         valXSteps = findValidXVelsWSteps x
         intersectionMap = Map.intersectionWith (\xs ys -> [(x,y) | x <- xs, y <- ys]) valXSteps valYSteps

findMaxYPos :: Int -> Int
findMaxYPos yVel = yPosAtStep yVel yVel

findMaxYVel :: (Int,Int) -> Int
findMaxYVel (y1,y2) = abs (min y1 y2) - 1

findMinYVel :: (Int,Int) -> Int
findMinYVel (y1,y2) = min y1 y2

yPosAtStep :: Int -> Int -> Int
yPosAtStep y n =  y * n - ((n - 1) * n `div` 2)

xPosAtStep :: Int -> Int -> Int
xPosAtStep x n | n > x = x * x - ((x - 1) * x `div` 2)
               | otherwise = x * n - ((n - 1) * n `div` 2)

findMinXVel :: (Int,Int) -> Int
findMinXVel (x1,x2) = head $ filter (\n -> (n `div` 2) * (n  + 1) > x1) [1..x1]

findMaxXVel :: (Int,Int) -> Int
findMaxXVel (x1,x2) = x2

findValidYVelsWSteps :: (Int,Int) -> StepValVel
findValidYVelsWSteps t = Map.unionsWith (++) maps
                               where
                                   yMax = findMaxYVel t
                                   yMin = findMinYVel t
                                   steps = map (findTimeYIntercepts t) [yMin .. yMax]
                                   maps = zipWith (curry createStepMaps) [yMin .. yMax] steps

createStepMaps :: (Int,[Int]) -> StepValVel
createStepMaps (vel,steps) = foldr (\s -> Map.insert s [vel]) Map.empty steps

findStepYPastRange :: Int -> (Int,Int) -> Int
findStepYPastRange 0 (y1,y2) = 1 + abs (min y1 y2)
findStepYPastRange yVel (y1,y2) = 2 * yVel + 1 + (abs (min y1 y2) `div` yVel)

findTimeYIntercepts :: (Int,Int) -> Int -> [Int]
findTimeYIntercepts (y1,y2) yVel | yVel >= 0 = filter (withinYTarget yVel (y1,y2)) [2 * yVel + 1 .. findStepYPastRange yVel (y1,y2)]
                                 | yVel < 0 = filter (withinYTarget yVel (y1,y2)) [1 .. abs (min y1 y2) `div` abs yVel]

findTimeXIntercepts :: (Int,Int) -> Int -> [Int]
findTimeXIntercepts (x1,x2) xVel | finalPos >= x1 && finalPos <= x2 = [head $ filter (withinXTarget xVel (x1,x2)) [xVel - 8 ..] .. 200]
                                 | otherwise = filter (withinXTarget xVel (x1,x2)) [1 .. 20]
                                 where
                                     finalPos = xPosAtStep xVel xVel

findValidXVelsWSteps :: (Int,Int) -> StepValVel
findValidXVelsWSteps t = Map.unionsWith (++) maps
                               where
                                   xMin = findMinXVel t
                                   xMax = findMaxXVel t
                                   steps = map (findTimeXIntercepts t) [xMin .. xMax]
                                   maps = zipWith (curry createStepMaps) [xMin .. xMax] steps

withinYTarget :: Int -> (Int,Int) -> Int -> Bool
withinYTarget yVel (y1,y2) step = yPos >= y1'  && yPos <= y2'
                                  where
                                      yPos = yPosAtStep yVel step
                                      y1' = min y1 y2
                                      y2' = max y1 y2

withinXTarget :: Int -> (Int,Int) -> Int -> Bool
withinXTarget xVel (x1,x2) step = xPos >= x1'  && xPos <= x2'
                                  where
                                      xPos = xPosAtStep xVel step
                                      x1' = min x1 x2
                                      x2' = max x1 x2

target :: ((Int,Int),(Int,Int))
target = ((20,30),(-10,-5))

target' :: ((Int,Int),(Int,Int))
target' = ((201,230),(-99,-65))