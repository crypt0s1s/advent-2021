module Main where

import Lib

import qualified Data.Map.Strict as Map
import Data.List.Split
import Data.Char
import Data.Maybe

type Path = [String]
type Cave = String
type Connections = [Cave]
type CaveMap = Map.Map Cave Connections

main :: IO ()
main = do
    inpTest <- lines <$> readFile "day12Test1.txt"
    inpTest1 <- lines <$> readFile "day12Test2.txt"
    inpTest2 <- lines <$> readFile "day12Test.txt"
    inp <- lines <$> readFile "day12.txt"
    print $ findPaths $ createCaveMap inpTest 
    print $ findPaths $ createCaveMap inpTest1
    print $ findPaths $ createCaveMap inpTest2
    print $ findPaths $ createCaveMap inp
    print $ findPaths' $ createCaveMap inpTest
    print $ findPaths' $ createCaveMap inpTest1
    print $ findPaths' $ createCaveMap inpTest2
    print $ findPaths' $ createCaveMap inp

createCaveMap :: [String] -> CaveMap
createCaveMap = foldr updateCaveMap Map.empty


updateCaveMap :: String -> CaveMap -> CaveMap
updateCaveMap connection caveMap = update2
                                   where
                                       [cave1,cave2] = splitOn "-" connection
                                       update1 = Map.insertWith (\[x] y -> x:y ) cave1 [cave2] caveMap
                                       update2 = Map.insertWith (\[x] y -> x:y ) cave2 [cave1] update1

findPaths :: CaveMap -> Int
findPaths cMap = findPathsInCave cMap [] "start"


findPathsInCave :: CaveMap -> Path -> Cave -> Int
findPathsInCave _ _ "end" = 1
findPathsInCave cMap path cave = sum $ map (findPathsInCave cMap (cave:path)) filteredConnections
                                    where
                                        connections = cMap Map.! cave
                                        filteredConnections = filter (\x -> isLargeCave x || x `notElem` path) connections

isLargeCave :: String -> Bool
isLargeCave = all isUpper

findPaths' :: CaveMap -> Int
findPaths' cMap = findPathsInCave' cMap [] (False,"start")

findPathsInCave' :: CaveMap -> Path -> (Bool,Cave) -> Int
findPathsInCave' _ _ (_,"end") = 1
findPathsInCave' cMap path (visitedtwice,cave) = sum $ map (findPathsInCave' cMap (cave:path)) filteredConnections
                                    where
                                        connections = cMap Map.! cave
                                        filteredConnections = mapMaybe (canTravelTo path) (zip (repeat visitedtwice) connections)

canTravelTo :: Path -> (Bool,Cave) -> Maybe (Bool,Cave)
canTravelTo path (_,"start") = Nothing
canTravelTo path (True,cave) | isLargeCave cave = Just (True,cave)
                             | cave `notElem` path = Just (True,cave)
                             | otherwise = Nothing
canTravelTo path (False,cave) | isLargeCave cave = Just (False,cave)
                              | cave `notElem` path = Just (False,cave)
                              | otherwise = Just (True,cave)