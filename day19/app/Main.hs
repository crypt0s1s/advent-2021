module Main where

import Lib
import Data.List.Split
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import Data.List
import Data.Maybe
import GHC.Base (undefined)

type Point = (Int,Int,Int)
type Vector = (Int,Int,Int)
type Scanner = [Beacon]
type Distance = Int
type VectorSet = S.Set Vector
type Beacon = (Point,VectorSet,ManhattanMap)
type FullSet = MS.MultiSet Point
type ManhattanMap = MS.MultiSet Distance

main :: IO ()
main = do
         inp <- readFile "day19.txt"
         let scanners = parseInput inp
         let (scannerPos,completeSet) = mergeScanners scanners
         print $ p1 completeSet
         print $ p2 scannerPos

p1 :: FullSet -> Int
p1 = MS.distinctSize

p2 :: [Point] -> Int
p2 points = foldr (\p m -> max m (foldr (\p' m'-> max m' $ findMan p p') 0 points)) 0 points
            
mergeScanners :: [Scanner] -> ([Point],FullSet)
mergeScanners (s:scanners) = mergeScanners' [(0,0,0)] initialMS [s] scanners
                  where
                      initialMS = MS.fromList (map getFst s)

mergeScanners' :: [Point] -> FullSet -> [Scanner] -> [Scanner] -> ([Point],FullSet)
mergeScanners' sPos fs _ [] = (sPos,fs)
mergeScanners' sPos fs (s:jScanners) scanners = mergeScanners' sPos' fs' jScanners' unjScanners
                    where
                        (sPos',fs',jScanners',unjScanners) = foldr (tryJoinScanners s) (sPos,fs,jScanners,[]) scanners

tryJoinScanners :: Scanner -> Scanner -> ([Point],FullSet,[Scanner],[Scanner]) -> ([Point],FullSet,[Scanner],[Scanner])
tryJoinScanners s1 s2 (scannerPos,fs,jScanners,unjScanners) | isOver = (scannerPoint : scannerPos,updateFullSet fs s2',s2':jScanners,unjScanners)
                                                 | otherwise = (scannerPos,fs,jScanners,s2:unjScanners)
                                                    where
                                                        (s2', isOver, scannerPoint) = isOverlap s1 s2

updateFullSet :: FullSet -> Scanner -> FullSet
updateFullSet = foldr (MS.insert . getFst)

parseInput :: String -> [Scanner]
parseInput ls = map createScanners splitText
                where
                    splitText = splitOn "\n\n" ls

createScanners :: String -> Scanner
createScanners ls = pointsToScanner beacons
                    where
                        liness = drop 1 $ lines ls
                        beacons = map (\l -> let p = map read (splitOn "," l) in (head p, p !! 1, p !! 2)) liness

pointsToScanner :: [Point] -> Scanner
pointsToScanner beacons = map (\p -> let (s,m) = foldr (updateMaps p) (S.empty,MS.empty) beacons in (p,s,m)) beacons

updateMaps :: Point -> Point -> (VectorSet,ManhattanMap) -> (VectorSet,ManhattanMap)
updateMaps p p' (vs,mm) = (S.insert v vs, MS.insert m mm)
                          where
                              (v,m) = findVectorMan p p'

isOverlap :: Scanner -> Scanner -> (Scanner,Bool,Point)
isOverlap s1 s2 = overlap
    where
        isPotOverlap (p,vs,mm) = filter thrd $ map (\(p',vs',mm') -> (p,p',isPotentialOverlap mm mm')) s2
        potentialOverlaps = drop 11 $ concatMap isPotOverlap s1
        overlap = foldr (confirmUpdateOverlap s1) (s2,False,(0,0,0)) potentialOverlaps

isPotentialOverlap :: ManhattanMap -> ManhattanMap -> Bool
isPotentialOverlap mm mm' = 12 <= MS.size (MS.intersection mm mm')

confirmUpdateOverlap :: Scanner -> (Point,Point,Bool) -> (Scanner,Bool,Point) -> (Scanner,Bool,Point)
confirmUpdateOverlap _ _ (s,True,scannerPos) = (s,True,scannerPos)
confirmUpdateOverlap s1 (p1,p2,_) (s2,_,_) | isNothing correctRotation' = (s2,False,(0,0,0))
                                           | otherwise = (s2',True,scannerPos)
                                             where
                                                 (_,vSet1,_) = head $ filter (\x -> getFst x == p1) s1
                                                 (_,vSet2,_) = head $ filter (\x -> getFst x == p2) s2
                                                 rotatedVectors = transpose $ map getRotations $ S.toList vSet2
                                                 (x2,y2,z2) = p2
                                                 scannerVector = (-x2,-y2,-z2)

                                                 rotatedSets = vSet2 : map S.fromList rotatedVectors
                                                 correctRotation' = findCorrectRotation (-1) vSet1 rotatedSets
                                                 Just (rotationUsed,correctVectors) = correctRotation'
                                                 scannerVector' | rotationUsed == (-1) = scannerVector
                                                                | otherwise = (getRotations scannerVector) !! rotationUsed
                                                 scannerPos = addPointVector p1 scannerVector'
                                                 beacons = S.toList $ correctVectors
                                                 beacons' = map (addPointVector p1) beacons
                                                 s2' = pointsToScanner beacons'

findCorrectRotation :: Int -> VectorSet -> [VectorSet] -> Maybe (Int,VectorSet)
findCorrectRotation _ _ [] = Nothing
findCorrectRotation n vs (v:vs2) | 12 <= S.size (S.intersection vs v) = Just (n,v)
                                 | otherwise = findCorrectRotation (n + 1) vs vs2

addPointVector :: Point -> Vector -> Point
addPointVector (x,y,z) (x',y',z') = (x + x', y + y', z + z')

thrd :: (a,b,c) -> c
thrd (_,_,c) = c

getFst :: (a,b,c) -> a
getFst (a,_,_) = a

getRotations :: Vector -> [Vector]
getRotations (x,y,z) = [(x,-y,-z),(-x,y,-z),(-x,-y,z),(y,z,x),(y,-z,-x),(-y,z,-x),(-y,-z,x),(z,x,y),(z,-x,-y),(-z,x,-y),(-z,-x,y),(-x,-z,-y),(x,z,-y),(x,-z,y),(-x,z,y),(-y,-x,-z),(y,x,-z),(y,-x,z),(-y,x,z),(-z,-y,-x),(z,y,-x),(z,-y,x),(-z,y,x)]

findVectorMan :: Point -> Point -> (Vector,Distance)
findVectorMan (x,y,z) (x',y',z') = ((xD,yD,zD),abs xD + abs yD + abs zD)
                                   where
                                       xD = x' - x
                                       yD = y' - y
                                       zD = z' - z

findMan :: Point -> Point -> Distance
findMan (x,y,z) (x',y',z') = abs xD + abs yD + abs zD
                             where
                                 xD = x' - x
                                 yD = y' - y
                                 zD = z' - z