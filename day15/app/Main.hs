module Main where

import Lib
import qualified Data.PQueue.Min as Q
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (digitToInt)
import Data.Maybe (isJust)

type Position = (Int,Int)
type RiskLevel = Int
type RiskTotal = Int
type ManhatanDistance = Int
type CavernMap = Map.Map Position RiskLevel
type VistedPoints = Set.Set Position
data PositionCost = PD {
    position :: Position,
    riskTotal :: RiskTotal,
    distance :: ManhatanDistance
}
  deriving Show

type PQ = Q.MinQueue PositionCost

instance Eq PositionCost where
    p == p' = position p == position p' && riskTotal p == riskTotal p' && distance p == distance p'

instance Ord PositionCost where
    p <= p' = (riskTotal p + distance p) <= (riskTotal p' + distance p')

main :: IO ()
main = do
         inp <- lines <$> readFile "day15.txt"
         let (cMap,target) = createCavernMap inp
         print $ p1 cMap target
         let (cMap',target') = createCavernMap' inp
         print $ p2 cMap' target'

p1 :: CavernMap -> Position -> Int
p1 cMap target = aStarSearch cMap pq vp target
                 where
                     pq = Q.singleton (PD (0,0) 0 0)
                     vp = Set.empty

p2 :: CavernMap -> Position -> Int
p2 cMap target = aStarSearch cMap pq vp target
                 where
                     pq = Q.singleton (PD (0,0) 0 0)
                     vp = Set.empty

createCavernMap :: [String] -> (CavernMap,Position)
createCavernMap ls = (cMap,(maxX,maxY))
                     where
                         maxX = (length $ head ls) - 1
                         maxY = (length ls) - 1
                         cMap = Map.fromList $ zip [ (x,y) | y <- [0 .. maxY], x <- [0 .. maxX] ] $ map digitToInt $ concat ls

createCavernMap' :: [String] -> (CavernMap,Position)
createCavernMap' ls = (cMap,(l,l))
                      where
                         l = (length ls) * 5 - 1
                         single = length ls
                         intList = map (map digitToInt) ls
                         incrementLists = map (incrementCavernMapN intList) [1 .. 8]
                         ranges = [ (((x,x + single - 1),(y,y + single - 1)),(x `div` 10) + (y `div` 10)) | y <- [0,single .. single * 4], x <- [0,single .. single * 4] ]
                         cMap = Map.unions $ map (Map.fromList . (generatePoints intList)) ranges

generatePoints :: [[Int]] -> (((Int,Int),(Int,Int)),Int) -> [(Position,RiskLevel)]
generatePoints cave (((minX,maxX),(minY,maxY)),n) = zip [ (x,y) | y <- [minY .. maxY], x <- [minX .. maxX] ] $ concat $ incrementCavernMapN cave n

incrementCavernMapN :: [[Int]] -> Int -> [[Int]]
incrementCavernMapN rs i = map (map (incrementCavernMapN' i)) rs

incrementCavernMapN' :: Int -> Int -> Int
incrementCavernMapN' i r = (+) 1 $ (r + i - 1) `mod` 9

incrementCavernMap :: Int -> Int
incrementCavernMap r = (+) 1 $ r `mod` 9

aStarSearch :: CavernMap -> PQ -> VistedPoints -> Position -> Int
aStarSearch cMap pq vPoints target | position minElment == target = riskTotal minElment -- if element is the destination
                                   | Set.member (position minElment) vPoints = aStarSearch cMap restPQ vPoints target -- if element has been visited already skip
                                   | otherwise = aStarSearch cMap updatedPQ vPoints' target
                             where
                                (minElment,restPQ) = Q.deleteFindMin pq
                                adjacentPointsPos =  map (tupleAdd $ position minElment) [(0,1),(1,0),(-1,0),(0,-1)]
                                adjacentPoints = zip adjacentPointsPos $ map (cMap Map.!?) adjacentPointsPos
                                adjacentPoints' = filter (\p -> isJust (snd p) && notVisited vPoints p) adjacentPoints
                                updatedPQ = foldr (\(p,Just r) -> Q.insert (createPositionCost p target r (riskTotal minElment))) restPQ adjacentPoints'
                                vPoints' = Set.insert (position minElment) vPoints

createPositionCost :: Position -> Position -> RiskLevel -> RiskTotal -> PositionCost
createPositionCost pos target riskL riskT = PD pos (riskL + riskT) (calcManhatan pos target)

calcManhatan :: Position -> Position -> ManhatanDistance
calcManhatan (x,y) (x',y') = abs (x - x') + abs (y' - y)

notVisited :: VistedPoints -> (Position, Maybe RiskLevel) -> Bool
notVisited _ (_,Nothing) = False
notVisited vPoints (p,_) = Set.notMember p vPoints

tupleAdd :: (Int,Int) -> (Int,Int) -> (Int,Int)
tupleAdd (x,y) (x',y') = (x + x', y + y')
