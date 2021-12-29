module Main where

import Lib
import qualified Data.Set as Set
import Data.Char

type Point = (Int,Int)
type LightSet = Set.Set Point
type EnchancementSet = Set.Set Int
data LightSetContainer = LS Int Int Int Int LightSet

main :: IO ()
main = do
         inp <- lines <$> readFile "day20.txt"
         print $ noLights $ enchanceImageNTimes (generateLightSet $ drop 2 inp) (createEnchancementSet $ head inp) 2
         print $ noLights $ enchanceImageNTimes (generateLightSet $ drop 2 inp) (createEnchancementSet $ head inp) 50

instance Show LightSetContainer where
    show (LS _ _ _ _ set) = show set

createEnchancementSet :: String -> EnchancementSet
createEnchancementSet xs = Set.fromList $ map snd $ filter ((== '#') . fst) z
                           where
                               z = zip xs [0..]

noLights :: LightSetContainer -> Int
noLights (LS xMin yMin xMax yMax set) = Set.size set

fromBits :: [Int] -> Int
fromBits = foldl (\t x -> t * 2 + x) 0

generateLightSet :: [String] -> LightSetContainer
generateLightSet xs = LS 0 0 xMax yMax (Set.unions $ zipWith generateLightSet' xs [0..])
                      where
                          xMax = (length $ head xs) - 1
                          yMax = length xs - 1

generateLightSet' :: String -> Int -> LightSet
generateLightSet' xs i = Set.fromList $ zip (map snd $ filter (\x -> fst x == '#') (zip xs [0..])) (repeat i)

lightSetToString :: LightSetContainer -> String
lightSetToString (LS xMin yMin xMax yMax set) = unlines result
                       where
                          result = map result' [yMin .. yMax]
                          result' y = map (result'' y) [xMin .. xMax]
                          result'' y x | Set.member (x,y) set = '#'
                                       | otherwise = '.'

enchanceImageNTimes :: LightSetContainer -> EnchancementSet -> Int -> LightSetContainer
enchanceImageNTimes lSetC eSet 0 = lSetC
enchanceImageNTimes lSetC eSet n = enchanceImageNTimes (LS xMin yMin xMax yMax lSet) eSet (n - 1)
                    where
                        (LS xMin yMin xMax yMax lSet) = enchanceImage (n `mod` 2) lSetC eSet
                        onList = Set.toList lSet
                        (xMin',yMin') = foldl findMinXY (head onList) onList
                        (xMax',yMax') = foldl findMaxXY (head onList) onList

enchanceImage :: Int -> LightSetContainer -> EnchancementSet -> LightSetContainer
enchanceImage outsideVal (LS xMin yMin xMax yMax lSet) eSet = LS minX minY maxX maxY newLSet
            where
                points = [ (x,y) | x <- [xMin - 1 .. xMax + 1], y <- [yMin - 1 .. yMax + 1] ]
                newLSet = foldl (enchanceImage' outsideVal (LS xMin yMin xMax yMax lSet) eSet) Set.empty points 
                pointList = Set.toList newLSet
                (minX,minY) = foldl findMinXY (head pointList) pointList
                (maxX,maxY) = foldl findMaxXY (head pointList) pointList

enchanceImage' :: Int -> LightSetContainer -> EnchancementSet -> LightSet -> Point -> LightSet
enchanceImage' outsideVal (LS xMin yMin xMax yMax lSet) eSet newLSet (x,y) | isMember = Set.insert (x,y) newLSet
                                       | otherwise = newLSet
               where
                   adjacentPoints = [ (x + x',y + y') | y' <- [-1..1], x' <- [-1..1]]
                   val = fromBits $ map pointValue adjacentPoints
                   pointValue (x,y) | x < xMin || x > xMax || y < yMin || y > yMax = outsideVal
                                    | Set.member (x,y) lSet = 1
                                    | otherwise = 0
                   isMember = Set.member val eSet

findMinXY :: (Int,Int) -> (Int,Int) -> (Int,Int)
findMinXY (minX,minY) (x,y) | x < minX && y < minY = (x,y)
                            | x < minX = (x,minY)
                            | y < minY = (minX,y)
                            | otherwise = (minX,minY)

findMaxXY :: (Int,Int) -> (Int,Int) -> (Int,Int)
findMaxXY (maxX,maxY) (x,y) | x > maxX && y > maxY = (x,y)
                            | x > maxX = (x,maxY)
                            | y > maxY = (maxX,y)
                            | otherwise = (maxX,maxY)
