module Main where

import Lib
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.PQueue.Min as Q
import Debug.Trace

type Position = (Int,Int)
data Amphipod = A Position |
                B Position | 
                C Position | 
                D Position
                deriving (Eq,Show)

type Cost = Int
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

endMap :: BurrowMap
endMap = M.fromList $ [((2,1),'A'),((2,2),'A'),((4,1),'B'),((4,2),'B'),((6,1),'C'),((6,2),'C'),((8,1),'D'),((8,2),'D')]

endMap2 :: BurrowMap
endMap2 = M.fromList $ [((2,1),'A'),((2,2),'A'),((2,3),'A'),((2,4),'A'),((4,1),'B'),((4,2),'B'),((4,3),'B'),((4,4),'B'),((6,1),'C'),((6,2),'C'),((6,3),'C'),((6,4),'C'),((8,1),'D'),((8,2),'D'),((8,3),'D'),((8,4),'D')]

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
{-
findValidMoves :: BurrowMap -> Amphipod -> [Move]
findValidMoves bMap amphipod | inRoom && (finalPos || stuck) = []
                             | inRoom && isHomeRoomAvailable && canMoveToHomeRoomFromRoom = [homeRoomMove] 
                             | inRoom = validHallwayMoves
                             | isHomeRoomAvailable && canMoveToHomeRoom = [homeRoomMove]
                             | otherwise = []
                               where
                                   (x,y) = getPos amphipod
                                   inRoom = y > 0
                                   homeX = getHomeX amphipod
                                   amphipodType = getType amphipod
                                   finalPos = x == homeX && (y == 2 || (y == 1 && Just amphipodType == bMap M.!? (homeX,2)))
                                   stuck = y == 2 && M.member (x,1) bMap

                                   isHomeRoomAvailable = (M.notMember (homeX,2) bMap && M.notMember (homeX,1) bMap) || (bMap M.!? (homeX,2) == Just amphipodType && M.notMember (homeX,1) bMap)
                                   canMoveToHomeRoomFromRoom = all (\x' -> M.notMember (x',0) bMap) $ enumFromThenTo (min x homeX + 1) (min x homeX + 3) (max x homeX - 1)
                                   homeRoomMove | M.member (homeX,2) bMap = (amphipod,createAmphipod amphipodType (homeX,1), getCost amphipod (homeX,1))
                                                | otherwise = (amphipod,createAmphipod amphipodType (homeX,2), getCost amphipod (homeX,2))
 
                                   (hallwayBehind,hallwayInfront) = break (> x) [0,1,3,5,7,9,10]
                                   validHallwayBehind = takeWhile (\n -> M.notMember (n,0) bMap) (reverse hallwayBehind)
                                   validHallwayInfront = takeWhile (\n -> M.notMember (n,0) bMap) hallwayInfront
                                   validHallwayMoves = map (\p -> (amphipod,createAmphipod amphipodType p, getCost amphipod p)) $ zip (validHallwayBehind ++ validHallwayInfront) (repeat 0)

                                   canMoveToHomeRoom | x < homeX = all (\x' -> M.notMember (x',0) bMap) $ enumFromThenTo (x + 2) (x + 4) (homeX - 1)
                                                     | otherwise = all (\x' -> M.notMember (x',0) bMap) $ enumFromThenTo (homeX + 1) (homeX + 3) (x - 2)
-}
main :: IO ()
main = do
    print p2

p1 :: Int
p1 = aStarSearch pq S.empty
     where
         pq = Q.singleton initialState

p1Test :: Int
p1Test = aStarSearch pq S.empty
         where
             pq = Q.singleton initialTestState

p2 :: Int
p2 = aStarSearch pq S.empty
     where
         pq = Q.singleton initialState2

p2Test :: Int
p2Test = aStarSearch pq S.empty
         where
             pq = Q.singleton initialTestState2

initialTestState :: State
initialTestState = State (initialiseBurrowMap aimphipods) 0 minCost
                   where
                       aimphipods = [B (2,1), A (2,2), C (4,1), D (4,2), B (6,1), C (6,2), D (8,1), A (8,2)]
                       minCost = sum $ map (\a -> (getCost a (getHomeX a,1))) aimphipods

initialState :: State
initialState = State (initialiseBurrowMap aimphipods) 0 minCost
               where
                   aimphipods = [A (4,1), A (4,2), B (6,1), B (8,2), C (2,1), C (2,2), D (8,1), D (6,2)]
                   minCost = sum $ map (\a -> (getCost a (getHomeX a,1))) aimphipods

initialState2 :: State
initialState2 = State (initialiseBurrowMap aimphipods) 0 minCost
                where
                    aimphipods = [A (4,1), A (4,4), A (6,3), A (8,2), B (4,3), B (6,1), B (6,2), B (8,4), C (2,1), C (2,4), C (4,2), C (8,3), D (2,2), D (2,3), D (6,4), D (8,1)]
                    minCost = sum $ map (\a -> (getCost a (getHomeX a,1))) aimphipods

initialTestState2 :: State
initialTestState2 = State (initialiseBurrowMap aimphipods) 0 minCost
                    where
                        aimphipods = [A (2,4), A (6,3), A (8,2), A (8,4), B (2,1), B (4,3), B (6,1), B (6,2), C (4,1), C (4,2), C (6,4), C (8,3), D (2,2), D (2,3), D (4,4), D (8,1)]
                        minCost = sum $ map (\a -> (getCost a (getHomeX a,1))) aimphipods

initialTestState2' :: State
initialTestState2' = State (initialiseBurrowMap aimphipods) 0 minCost
                     where
                         aimphipods = [A (0,0), A (2,2), A (2,3), A (2,4), B (4,1), B (4,2), B (4,3), B (4,4), C (6,1), C (6,2), C (6,3), C (6,4), D (3,0), D (10,0), D (8,3), D (8,4)]
                         minCost = sum $ map (\a -> (getCost a (getHomeX a,1))) aimphipods

aimphipodsFromMap :: BurrowMap -> [Amphipod]
aimphipodsFromMap bMap = map (\(p,t) -> createAmphipod t p) $ M.toList bMap

updateBMap :: BurrowMap -> Amphipod -> Amphipod -> BurrowMap
updateBMap bMap aInit aFinal = M.insert (getPos aFinal) (getType aFinal) $ M.delete (getPos aInit) bMap

updateState :: State -> Move -> State
updateState s (aInit,aFinal,c) = State updatedBMap (c + currentCost s) newMinCost
                                 where
                                    updatedBMap = updateBMap (mp s) aInit aFinal
                                    newMinCost = findMinCost updatedBMap
{-
findMinCost :: BurrowMap -> Int
findMinCost bMap = at + bt + ct + dt
                         where
                             apods = aimphipodsFromMap bMap
                             as = filter (\t -> getType t == 'A') apods
                             bs = filter (\t -> getType t == 'B') apods
                             cs = filter (\t -> getType t == 'C') apods
                             ds = filter (\t -> getType t == 'D') apods
                             at | getPos (as !! 0) == (2,2) = (getCost (as !! 0) (2,2)) + (getCost (as !! 1) (2,1))
                                | otherwise = (getCost (as !! 0) (2,1)) + (getCost (as !! 1) (2,2))
                             bt | getPos (bs !! 0) == (4,2) = (getCost (bs !! 0) (4,2)) + (getCost (bs !! 1) (4,1))
                                | otherwise = (getCost (bs !! 0) (4,1)) + (getCost (bs !! 1) (4,2))
                             ct | getPos (cs !! 0) == (6,2) = (getCost (cs !! 0) (6,2)) + (getCost (cs !! 1) (6,1))
                                | otherwise = (getCost (cs !! 0) (6,1)) + (getCost (cs !! 1) (6,2))
                             dt | getPos (ds !! 0) == (8,2) = (getCost (ds !! 0) (8,2)) + (getCost (ds !! 1) (8,1))
                                | otherwise = (getCost (ds !! 0) (8,1)) + (getCost (ds !! 1) (8,2))
-}

findMinCost :: BurrowMap -> Int
findMinCost bMap = at + bt + ct + dt
                    where
                        apods = aimphipodsFromMap bMap
                        as = filter (\t -> getType t == 'A') apods
                        bs = filter (\t -> getType t == 'B') apods
                        cs = filter (\t -> getType t == 'C') apods
                        ds = filter (\t -> getType t == 'D') apods
                        (aHome,aAway) = span (isInHomeRow 2) as
                        (bHome,bAway) = span (isInHomeRow 2) bs
                        (cHome,cAway) = span (isInHomeRow 2) cs
                        (dHome,dAway) = span (isInHomeRow 2) ds
                        unfilledPoints homeX homeA = S.toList $ foldr S.delete (S.fromList (zip (repeat homeX) [1 .. deepestRoom])) homeA
                        at = sum $ map (\(p1,p2) -> getCost p1 p2) $ zip aAway (unfilledPoints 2 (map getPos aAway))
                        bt = sum $ map (\(p1,p2) -> getCost p1 p2) $ zip bAway (unfilledPoints 4 (map getPos bAway))
                        ct = sum $ map (\(p1,p2) -> getCost p1 p2) $ zip cAway (unfilledPoints 6 (map getPos cAway))
                        dt = sum $ map (\(p1,p2) -> getCost p1 p2) $ zip dAway (unfilledPoints 8 (map getPos dAway))
                        isInHomeRow homeX a = let (x,y) = getPos a in x == homeX


aStarSearch :: PQ -> VisitedSet -> Int
aStarSearch pq visited | bMap == endMap2 = currentCost popped
                       | S.member bMap visited = aStarSearch restPQ visited
                       | otherwise = aStarSearch updatedPQ (S.insert bMap visited)
                         where
                            (popped,restPQ) = Q.deleteFindMin pq 
                            bMap = mp popped
                            aimphipods = aimphipodsFromMap bMap
                            moves = concatMap (findValidMoves bMap) aimphipods
                            futureStates = map (updateState popped) moves
                            updatedPQ = foldr Q.insert restPQ futureStates


deepestRoom :: Int
deepestRoom = 4


findValidMoves :: BurrowMap -> Amphipod -> [Move]
findValidMoves bMap amphipod | inRoom && (finalPos || stuck) = []
                             | inRoom && isHomeRoomAvailable && canMoveToHomeRoomFromRoom = [homeRoomMove] 
                             | inRoom = validHallwayMoves
                             | isHomeRoomAvailable && canMoveToHomeRoom = [homeRoomMove]
                             | otherwise = []
                               where
                                   (x,y) = getPos amphipod
                                   inRoom = y > 0
                                   homeX = getHomeX amphipod
                                   amphipodType = getType amphipod
                                   finalPos = x == homeX && all (\y' -> Just amphipodType == bMap M.!? (homeX,y')) [y + 1 .. deepestRoom]
                                   stuck = y > 1 && any (\y' -> M.member (x,y') bMap) [1 .. y - 1]
 
                                   isHomeRoomAvailable = all (\y' -> M.findWithDefault amphipodType (homeX,y') bMap == amphipodType) [1 .. deepestRoom]
                                   canMoveToHomeRoomFromRoom = all (\x' -> M.notMember (x',0) bMap) $ enumFromThenTo (min x homeX + 1) (min x homeX + 3) (max x homeX - 1)

                                   homeRoomMoveDest = head $ filter (flip (M.notMember) bMap) $ zip (repeat homeX) $ enumFromThenTo deepestRoom (deepestRoom - 1) 1
                                   homeRoomMove = (amphipod,createAmphipod amphipodType homeRoomMoveDest, getCost amphipod homeRoomMoveDest)
 
                                   (hallwayBehind,hallwayInfront) = break (> x) [0,1,3,5,7,9,10]
                                   validHallwayBehind = takeWhile (\n -> M.notMember (n,0) bMap) (reverse hallwayBehind)
                                   validHallwayInfront = takeWhile (\n -> M.notMember (n,0) bMap) hallwayInfront
                                   validHallwayMoves = map (\p -> (amphipod,createAmphipod amphipodType p, getCost amphipod p)) $ zip (validHallwayBehind ++ validHallwayInfront) (repeat 0)

                                   canMoveToHomeRoom | x < homeX = all (\x' -> M.notMember (x',0) bMap) $ enumFromThenTo (x + 2) (x + 4) (homeX - 1)
                                                     | otherwise = all (\x' -> M.notMember (x',0) bMap) $ enumFromThenTo (homeX + 1) (homeX + 3) (x - 2)



{-
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
-}



