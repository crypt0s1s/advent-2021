module Main where

import Lib
import Data.List.Split

type Area = ((Int,Int),(Int,Int),(Int,Int))
data Cuboid = Cuboid Bool Area
  deriving Show

main :: IO ()
main = do
         inp <- lines <$> readFile "day22.txt"
         let instructions = map createCuboid inp
         print $ p1 instructions
         print $ p2 instructions

p1 :: [Cuboid] -> Int
p1 cuboids = countOnCubes $ followInstructions $ filter in50 cuboids

p2 :: [Cuboid] -> Int
p2 cuboids = countOnCubes $ followInstructions cuboids

in50 :: Cuboid -> Bool
in50 (Cuboid _ ((x,x'),(y,y'),(z,z'))) = not $ x < (-50) || z < (-50) || z < (-50) || x' > 50 || y' > 50 || z' > 50

createCuboid :: String -> Cuboid
createCuboid line = Cuboid (t == "on") ((x1,x2),(y1,y2),(z1,z2))
                   where
                       [t,area] = words line
                       [[x1,x2],[y1,y2],[z1,z2]] = map (\n -> map read $ splitOn ".." $ drop 2 n) $ splitOn "," area

followInstructions :: [Cuboid] -> [Area]
followInstructions = foldl updateCubes []

updateCubes :: [Area] -> Cuboid -> [Area]
updateCubes areas (Cuboid s area) | s = area : newAreas
                                  | otherwise = newAreas
                              where
                                  newAreas = concatMap (updateArea area) areas

updateArea :: Area -> Area -> [Area]
updateArea a1 a2 | isOverlapping a1 a2 = a2'
                 | otherwise = [a2]
                   where
                       ((x1,x1'),(y1,y1'),(z1,z1')) = a1
                       ((x2,x2'),(y2,y2'),(z2,z2')) = a2
                       xLow  = if x2  < x1  then [((x2,x1 - 1),(y2,y2'),(z2,z2'))]                                   else []
                       xHigh = if x2' > x1' then [((x1' + 1,x2'),(y2,y2'),(z2,z2'))]                                 else []
                       yLow  = if y2  < y1  then [((max x1 x2, min x1' x2'),(y2,y1 - 1),(z2,z2'))]                   else []
                       yHigh = if y2' > y1' then [((max x1 x2, min x1' x2'),(y1' + 1,y2'),(z2,z2'))]                 else []
                       zLow  = if z2  < z1  then [((max x1 x2, min x1' x2'),(max y1 y2, min y1' y2'),(z2,z1 - 1))]   else []
                       zHigh = if z2' > z1' then [((max x1 x2, min x1' x2'),(max y1 y2, min y1' y2'),(z1' + 1,z2'))] else []
                       a2' = xLow ++ xHigh ++ yLow ++ yHigh ++ zLow ++ zHigh

countOnCubes :: [Area] -> Int
countOnCubes areas = sum $ map countOnInCuboid areas

countOnInCuboid :: Area -> Int
countOnInCuboid ((x,x'),(y,y'),(z,z')) = ((x' + 1) - x) * ((y' + 1) - y) * ((z' + 1) - z)

isOverlapping :: Area -> Area -> Bool
isOverlapping a1 a2 = xOverlap && yOverlap && zOverlap
                      where
                          ((x1,x1'),(y1,y1'),(z1,z1')) = a1
                          ((x2,x2'),(y2,y2'),(z2,z2')) = a2
                          xOverlap = x1' >= x2 && x2' >= x1
                          yOverlap = y1' >= y2 && y2' >= y1
                          zOverlap = z1' >= z2 && z2' >= z1
