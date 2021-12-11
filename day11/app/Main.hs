module Main where

import Lib
import qualified Data.Map.Strict as Map
import Data.Char ( digitToInt )
import Debug.Trace

type Point = (Int, Int)
type EnergyGrid = Map.Map Point Int
type Energy = Int
type Explosions = Int

main :: IO ()
main = do
    inpTest <- lines <$> readFile "day11Test.txt"
    inp <- lines <$> readFile "day11.txt"
    print $ countExplosionsToN (createEnergyGrid inpTest) 100 0
    print $ countExplosionsToN (createEnergyGrid inp) 100 0
    print $ findSyncExplosion (createEnergyGrid inpTest) 0 0
    print $ findSyncExplosion (createEnergyGrid inp) 0 0

createEnergyGrid :: [String] -> EnergyGrid
createEnergyGrid ls = Map.fromList pointEnergyTuples
                      where
                          energyRow = zip ls [0..]
                          pointEnergyTuples = concatMap createEnergyGrid' energyRow

createEnergyGrid' :: (String, Int) -> [(Point, Energy)]
createEnergyGrid' (ls,y) = zip [(x,y) | x <- [0..]] (map digitToInt ls)

countExplosionsToN :: EnergyGrid -> Int -> Explosions -> Int
countExplosionsToN _ 0 explosions = explosions
countExplosionsToN eGrid n explosions = countExplosionsToN updatedGrid (n - 1) (explosions + explosionsInStep)
                                       where
                                           (explosionsInStep, updatedGrid) = takeStep eGrid

findSyncExplosion :: EnergyGrid -> Int -> Explosions -> Int
findSyncExplosion _ n 100 = n
findSyncExplosion eGrid n explosions = findSyncExplosion updatedGrid (n + 1) explosionsInStep
                                       where
                                           (explosionsInStep, updatedGrid) = takeStep eGrid

takeStep :: EnergyGrid -> (Explosions, EnergyGrid)
takeStep eGrid = resetEnergy updatedGrid
                 where
                     pointList = [(x,y) | x <- [0..9], y <- [0..9]]
                     updatedGrid = foldr (\p g -> updatePoint g p (g Map.! p)) eGrid pointList
                     
updatePoint :: EnergyGrid -> Point -> Energy -> EnergyGrid
updatePoint eGrid p energy | energy + 1 == 10 = foldr (\point grid -> updatePoint grid point (grid Map.! point)) updatedGrid (adjacentPoints p)
                           | otherwise = updatedGrid
                        where
                            isValidPoint (a,b) = a < 10 && b < 10 && a >= 0 && b >= 0
                            adjacentPoints (x,y) = [(x + x',y+y') | y' <- [-1,0,1], x' <- [-1,0,1], isValidPoint (x + x',y + y'), x' /= 0 || y' /= 0]
                            updatedGrid = Map.insert p (energy + 1) eGrid

resetEnergy :: EnergyGrid -> (Explosions, EnergyGrid)
resetEnergy = Map.mapAccum needReset 0

needReset :: Int -> Energy -> (Int, Int)
needReset t e | e > 9 = (t + 1, 0) 
              | otherwise = (t, e)