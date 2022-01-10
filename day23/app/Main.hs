module Main where

import Lib
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.PQueue.Min as Q
import Debug.Trace
import GHC.Base (undefined)

type Position = (Int,Int)
data Amphipod = A Position |
                B Position |
                C Position |
                D Position
                deriving (Eq,Show)

type Cost = Int
type Depth = Int
type BurrowMap = M.Map Position Char
type VisitedSet = S.Set BurrowMap
type Move = (Amphipod,Amphipod,Cost)
data State = State {
  mp :: BurrowMap,
  currentCost :: Int,
  minExtraCost :: Int
}
  deriving (Eq,Show)
type PQ = Q.MinQueue State

instance Ord State where
    s1 <= s2 = currentCost s1 + minExtraCost s1 <= currentCost s2 + minExtraCost s2

initialiseBurrowMap :: [Amphipod] -> BurrowMap
initialiseBurrowMap amphipods = M.fromList amphipods'
                        where
                            amphipods' = zip (map getPos amphipods) $ map getType amphipods

getPos :: Amphipod -> Position
getPos (A p) = p
getPos (B p) = p
getPos (C p) = p
getPos (D p) = p

getHomeX :: Amphipod -> Int
getHomeX (A _) = 2
getHomeX (B _) = 4
getHomeX (C _) = 6
getHomeX (D _) = 8

getType :: Amphipod -> Char
getType (A _) = 'A'
getType (B _) = 'B'
getType (C _) = 'C'
getType (D _) = 'D'

createAmphipod :: Char -> Position -> Amphipod
createAmphipod 'A' p = A p
createAmphipod 'B' p = B p
createAmphipod 'C' p = C p
createAmphipod 'D' p = D p

getCost :: Amphipod -> Position -> Int
getCost apod dest | t == 'A' = 1 * cost
                  | t == 'B' = 10 * cost
                  | t == 'C' = 100 * cost
                  | t == 'D' = 1000 * cost
                  | otherwise = error "Not a valid type"
                    where
                        t = getType apod
                        cost = findCost (getPos apod) dest

findCost :: Position -> Position -> Int
findCost (x,y) (x',y') | x == x' && y == y' = 0
                       | otherwise = abs (x - x') + y + y'

inHomeRoom :: Amphipod -> Bool
inHomeRoom (A (x,y)) = x == 2
inHomeRoom (B (x,y)) = x == 4
inHomeRoom (C (x,y)) = x == 6
inHomeRoom (D (x,y)) = x == 8

main :: IO ()
main = do
    print p1
    print p2

p1 :: Int
p1 = aStarSearch 2 pq S.empty
     where
         pq = Q.singleton initialState

p1Test :: Int
p1Test = aStarSearch 2 pq S.empty
         where
             pq = Q.singleton initialTestState

p2 :: Int
p2 = aStarSearch 4 pq S.empty
     where
         pq = Q.singleton initialState2

p2Test :: Int
p2Test = aStarSearch 4 pq S.empty
         where
             pq = Q.singleton initialTestState2

initialTestState :: State
initialTestState = State (initialiseBurrowMap aimphipods) 0 minCost
                   where
                       aimphipods = [B (2,1), A (2,2), C (4,1), D (4,2), B (6,1), C (6,2), D (8,1), A (8,2)]
                       minCost = sum $ map (\a -> getCost a (getHomeX a,1)) aimphipods

initialState :: State
initialState = State (initialiseBurrowMap aimphipods) 0 minCost
               where
                   aimphipods = [A (4,1), A (4,2), B (6,1), B (8,2), C (2,1), C (2,2), D (8,1), D (6,2)]
                   minCost = sum $ map (\a -> getCost a (getHomeX a,1)) aimphipods

initialState2 :: State
initialState2 = State (initialiseBurrowMap aimphipods) 0 minCost
                where
                    aimphipods = [A (4,1), A (4,4), A (6,3), A (8,2), B (4,3), B (6,1), B (6,2), B (8,4), C (2,1), C (2,4), C (4,2), C (8,3), D (2,2), D (2,3), D (6,4), D (8,1)]
                    minCost = sum $ map (\a -> getCost a (getHomeX a,1)) aimphipods

initialTestState2 :: State
initialTestState2 = State (initialiseBurrowMap aimphipods) 0 minCost
                    where
                        aimphipods = [A (2,4), A (6,3), A (8,2), A (8,4), B (2,1), B (4,3), B (6,1), B (6,2), C (4,1), C (4,2), C (6,4), C (8,3), D (2,2), D (2,3), D (4,4), D (8,1)]
                        minCost = sum $ map (\a -> getCost a (getHomeX a,1)) aimphipods

initialTestState2' :: State
initialTestState2' = State (initialiseBurrowMap aimphipods) 0 minCost
                     where
                         aimphipods = [A (0,0), A (2,2), A (2,3), A (2,4), B (4,1), B (4,2), B (4,3), B (4,4), C (6,1), C (6,2), C (6,3), C (6,4), D (3,0), D (10,0), D (8,3), D (8,4)]
                         minCost = sum $ map (\a -> getCost a (getHomeX a,1)) aimphipods

aimphipodsFromMap :: BurrowMap -> [Amphipod]
aimphipodsFromMap bMap = map (\(p,t) -> createAmphipod t p) $ M.toList bMap

updateBMap :: BurrowMap -> Amphipod -> Amphipod -> BurrowMap
updateBMap bMap aInit aFinal = M.insert (getPos aFinal) (getType aFinal) $ M.delete (getPos aInit) bMap

updateState :: State -> Move -> State
updateState s (aInit,aFinal,c) = State updatedBMap (c + currentCost s) newMinCost
                                 where
                                    updatedBMap = updateBMap (mp s) aInit aFinal
                                    newMinCost = findMinCost updatedBMap

findMinCost :: BurrowMap -> Int
findMinCost bMap = sum $ map findCost' apods
                    where
                        apods = aimphipodsFromMap bMap

findCost' :: Amphipod -> Int
findCost' (A p) | fst p == 2 = 0
                | otherwise = getCost (A p) (2,0)
findCost' (B p) | fst p == 4 = 0
                | otherwise = getCost (B p) (4,0)
findCost' (C p) | fst p == 6 = 0
                | otherwise = getCost (C p) (6,0)
findCost' (D p) | fst p == 8 = 0
                | otherwise = getCost (D p) (8,0)

aStarSearch :: Depth -> PQ -> VisitedSet -> Int
aStarSearch depth pq visited | all inHomeRoom aimphipods = currentCost popped
                             | S.member bMap visited = aStarSearch depth restPQ visited
                             | otherwise = aStarSearch depth updatedPQ (S.insert bMap visited)
                               where
                                  (popped,restPQ) = Q.deleteFindMin pq
                                  bMap = mp popped
                                  aimphipods = aimphipodsFromMap bMap
                                  moves = concatMap (findValidMoves depth bMap) aimphipods
                                  futureStates = map (updateState popped) moves
                                  updatedPQ = foldr Q.insert restPQ futureStates

findValidMoves :: Depth -> BurrowMap -> Amphipod -> [Move]
findValidMoves depth bMap amphipod | inRoom && (finalPos || stuck) = []
                                   | inRoom && isHomeRoomAvailable && canMoveToHomeRoomFromRoom = [homeRoomMove]
                                   | inRoom = validHallwayMoves
                                   | isHomeRoomAvailable && canMoveToHomeRoom = [homeRoomMove]
                                   | otherwise = []
                                     where
                                         (x,y) = getPos amphipod
                                         inRoom = y > 0
                                         homeX = getHomeX amphipod
                                         amphipodType = getType amphipod
                                         finalPos = x == homeX && all (\y' -> Just amphipodType == bMap M.!? (homeX,y')) [y + 1 .. depth]
                                         stuck = y > 1 && any (\y' -> M.member (x,y') bMap) [1 .. y - 1]

                                         isHomeRoomAvailable = all (\y' -> M.findWithDefault amphipodType (homeX,y') bMap == amphipodType) [1 .. depth]
                                         canMoveToHomeRoomFromRoom = all (\x' -> M.notMember (x',0) bMap) $ enumFromThenTo (min x homeX + 1) (min x homeX + 3) (max x homeX - 1)

                                         homeRoomMoveDest = head $ filter (`M.notMember` bMap) $ zip (repeat homeX) $ enumFromThenTo depth (depth - 1) 1
                                         homeRoomMove = (amphipod,createAmphipod amphipodType homeRoomMoveDest, getCost amphipod homeRoomMoveDest)

                                         (hallwayBehind,hallwayInfront) = break (> x) [0,1,3,5,7,9,10]
                                         validHallwayBehind = takeWhile (\n -> M.notMember (n,0) bMap) (reverse hallwayBehind)
                                         validHallwayInfront = takeWhile (\n -> M.notMember (n,0) bMap) hallwayInfront
                                         validHallwayMoves = map (\x' -> (amphipod,createAmphipod amphipodType (x',0), getCost amphipod (x',0))) (validHallwayBehind ++ validHallwayInfront)

                                         canMoveToHomeRoom | x < homeX = all (\x' -> M.notMember (x',0) bMap) $ enumFromThenTo (x + 2) (x + 4) (homeX - 1)
                                                           | otherwise = all (\x' -> M.notMember (x',0) bMap) $ enumFromThenTo (homeX + 1) (homeX + 3) (x - 2)
