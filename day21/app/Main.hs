module Main where

import Lib
import Debug.Trace
import qualified Data.Map.Strict as Map


main :: IO ()
main = do
         print p1
         print p2

type Score = Int
type Position = Int
type Rolls = Int
data Turn = P1 | P2
data Player = Player Position Score
data Game = Game Rolls Turn Player Player

type QMap = Map.Map (Player,Player) (Int,Int)

instance Eq Turn where
  P1 == P1 = True
  P2 == P2 = True
  p == p' = False

instance Eq Player where
    Player p1 s1 == Player p2 s2 = p1 == p2 && s1 == s2

instance Ord Player where
    Player p1 s1 <= Player p2 s2 = s2 > s1 || (s2 == s1 && p2 >= p1)

instance Show Player where
    show (Player p s) = "Player: pos " ++ show p ++ " score " ++ show s

p1 :: (Int,Int)
p1 = (,) r losingPlayersPoints
    where
        (Game r _ p1 p2) = playGame (Game 0 P1 (Player 1 0) (Player 2 0))
        losingPlayersPoints = findLowestPoints p1 p2

findLowestPoints :: Player -> Player -> Int
findLowestPoints (Player _ s) (Player _ s') | s > s' = s'
                                            | otherwise = s

findHighestPoints :: Player -> Player -> Int
findHighestPoints (Player _ s) (Player _ s') | s > s' = s
                                             | otherwise = s'

playGame :: Game -> Game
playGame game | noWinner = playGame $ takeTurn game
              | otherwise = game
                where
                    (Game r _ p1 p2) = game
                    noWinner = 1000 > findHighestPoints p1 p2

getPoints :: Player -> Int
getPoints (Player _ s) = s

takeTurn :: Game -> Game
takeTurn (Game r p p1 p2) | p == P1 = Game (r + 3) P2 (movePlayer p1 relativeRoll) p2
                          | otherwise = Game (r + 3) P1 p1 (movePlayer p2 relativeRoll)
                            where
                               relativeRoll = rollDice (findNextRoll r) `mod` 10

movePlayer :: Player -> Int -> Player
movePlayer (Player pos score) relativeRoll | newPos == 0 = Player 10 (score + 10)
                                           | otherwise = Player newPos (score + newPos)
                                      where
                                         newPos = (pos + relativeRoll) `mod` 10

findNextRoll :: Rolls -> Int
findNextRoll r = (r `mod` 100) + 1

rollDice :: Int -> Int
rollDice x = sum $ take 3 nums
            where nums = [x .. 100] ++ [1 .. 99]

p2 :: (Int,Int)
p2 = playQuantumGame (Map.singleton (Player 1 0, Player 2 0) (1,0)) (0,0)

rollQuantum :: [(Int,Int)]
rollQuantum = [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]

playQuantumGame :: QMap -> (Int,Int) -> (Int,Int)
playQuantumGame qMap (s1,s2) | Map.null qMap = (s1,s2)
                             | otherwise = playQuantumGame updatedQMap (s1 + s1', s2 + s2')
                               where
                                   (updatedQMap,(s1',s2')) = playQuantumGame' qMap

playQuantumGame' :: QMap -> (QMap,(Int,Int))
playQuantumGame' qMap = (updatedQMap,wUv)
                                  where
                                      (lowestElm,rest) = Map.splitAt 1 qMap
                                      [((p1,p2),uv)] = Map.toList lowestElm
                                      (updatedQMap,wUv) = takeQuantumTurn uv p1 p2 rest 


-- r1,r2 stand for reached player 1 and reached player 2, aka the number of times
-- that each player reached that particular state
takeQuantumTurn :: (Int,Int) -> Player -> Player -> QMap -> (QMap,(Int,Int))
takeQuantumTurn (r1,r2) (Player p s) p2 qMap = (updatedQMap,(r1 * t, r2 * t))
                             where
                                newPositions = map (\(a,b) -> (findNewPos p a,b)) rollQuantum
                                knownWinningPositions = filter (\x -> fst x + s >= winningScore) newPositions
                                t = sum (map snd knownWinningPositions) 

                                unknownPositions = filter (\x -> fst x + s < winningScore) newPositions 
                                updatedQMap = foldr (\(a,b) m -> Map.insertWith addTuples (p2, Player a (a + s)) (r2 * b, r1 * b) m) qMap unknownPositions

addTuples :: (Int,Int) -> (Int,Int) -> (Int,Int)
addTuples (a,b) (a',b') = (a + a', b + b')

findNewPos :: Int -> Int -> Int
findNewPos p r = 1 + ((p + r - 1) `mod` 10)

winningScore :: Int
winningScore = 21
