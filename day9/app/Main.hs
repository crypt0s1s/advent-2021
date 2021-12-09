module Main where

import Lib

-- import qualified Data.Map as Map
import qualified Data.Map.Strict as Map
import Data.Char ( digitToInt )
import Data.Maybe ( mapMaybe )
import Data.List ( sort )

main :: IO ()
main = do
    inpTest <- lines <$> readFile "day9Test.txt"
    inp <- lines <$> readFile "day9.txt"
    print $ findLowPointsSum $ createDangerMap inpTest
    print $ findLowPointsSum $ createDangerMap inp
    print $ findTop3BasinSizesProduct $ createDangerMap inpTest
    print $ findTop3BasinSizesProduct $ createDangerMap inp

type Point = (Int,Int)
type DangerMap = Map.Map Point Int
type BasinMap =  Map.Map Point Point

createDangerMap :: [String] -> DangerMap
createDangerMap ls = Map.fromList $ concat $ zipWith zipPoints ls [0..]

zipPoints :: String -> Int -> [((Int,Int),Int)]
zipPoints ls y = zipWith (\l x -> ((x,y),digitToInt l)) ls [0..]

findLowPointsSum :: DangerMap -> Int
findLowPointsSum m = Map.foldlWithKey (findIfLowThenAdd m) 0 m

-- if a low point add to total sum
findIfLowThenAdd :: DangerMap -> Int -> (Int,Int) -> Int -> Int
findIfLowThenAdd m total (x,y) rating | isLow m (x,y) rating = 1 + rating + total
                                      | otherwise = total

-- Find if the point is a low point
isLow :: DangerMap -> (Int,Int) -> Int -> Bool
isLow m (x,y) rating = all (rating <) adjacentPointsRatings
                    where adjacentPoints = [ (x + x',y + y') |x' <- [-1,0,1], y' <- [-1,0,1], abs x' /= abs y']
                          adjacentPointsRatings = map (\p -> Map.findWithDefault 9 p m) adjacentPoints

-- Finds the 3 largest basins and calculates the product
findTop3BasinSizesProduct :: DangerMap -> Int
findTop3BasinSizesProduct dMap = product $ take 3 $ reverse $ sort $ map snd $ Map.toList $ countMap pointList
                      where
                          basinMap =  makeBasinMap dMap
                          pointList = map snd $ Map.toList basinMap

-- creates a map counting how many instances of each point there is
countMap :: [Point] -> Map.Map Point Int
countMap points = foldl countMap' Map.empty (zip points (repeat 1))

countMap' :: Map.Map Point Int -> (Point, Int) -> Map.Map Point Int
countMap' m (p, i) = Map.insertWith (+) p i m

-- createss the basin map detailing all the non 9 rating points and the lowest point in its basin
makeBasinMap :: DangerMap -> BasinMap
makeBasinMap dMap = foldl (updateBasinMap' dMap) basinMapInitial points
               where
                   lowPoints = map fst $ filter (uncurry (isLow dMap)) $ Map.toList dMap
                   basinMapInitial = Map.fromList $ zip lowPoints lowPoints
                   points = Map.toList dMap

-- updates the basin map for a particular point and that points lower adjacent points
updateBasinMap' :: DangerMap -> BasinMap -> (Point, Int) -> BasinMap
updateBasinMap' dMap bMap ((x,y), rating) | rating == 9 = bMap
                                          | Map.member (x,y) bMap = bMap
                                          | otherwise = Map.insert (x, y) lowerPointLowPoint adjacentLowerPointMap
                              where
                                  -- grab the list of adjacent points that are lower
                                  adjacentLowerPoints = filter (\p -> Map.findWithDefault 9 p dMap < rating) [ (x + x',y + y') |x' <- [-1,0,1], y' <- [-1,0,1], abs x' /= abs y']
                                  -- uses the current basin map and updates it with the info from the lower adjacent points
                                  adjacentLowerPointMap = foldl (updateBasinMap'' dMap) bMap adjacentLowerPoints
                                  -- finds the low point of the basin
                                  lowerPointLowPoint = adjacentLowerPointMap Map.! head adjacentLowerPoints

--  calls updateBasinMap' for a point
updateBasinMap'' :: DangerMap -> BasinMap -> Point -> BasinMap
updateBasinMap'' dMap bMap point = updateBasinMap' dMap bMap (point, dMap Map.! point)
