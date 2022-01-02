module Main where

import Lib (someFunc)
import qualified Data.Set as S
import GHC.Base (undefined)
import Data.String (String)
import Data.List (intercalate)
import Data.List.Split ( chunksOf )

data Type = E | S
  deriving (Eq,Show)

type Point = (Int,Int)
type CucumberSet = S.Set Point

main :: IO ()
main = do
         inp <- lines <$> readFile "day25.txt"
         inpTest <- lines <$> readFile "day25.test.txt"
         let (dims,sets) = parseInput inp
         putStr $ showCucumbers dims sets
         putStr "\n\n"
         putStr $ showCucumbers dims $ snd $ takeStep dims sets
         putStr "\n\n"
         let (steps,finalSets) = takeStepsTillStop 1 dims sets
         print steps
         putStr $ showCucumbers dims finalSets

showCucumbers :: (Int,Int) -> (CucumberSet,CucumberSet) -> String
showCucumbers (w,h) (cSetE,cSetW) = output'
                                    where
                                        points = [ (x,y) | y <- [0 .. h - 1], x <- [0 .. w - 1] ]
                                        output = map findValue points
                                        findValue p | S.member p cSetE = '>'
                                                    | S.member p cSetW = 'v'
                                                    | otherwise = '.'
                                        output' = intercalate "\n" $ chunksOf w output
                                        
parseInput :: [String] -> ((Int,Int),(CucumberSet,CucumberSet))
parseInput inp = ((width,height),(cucumberE,cucumberS))
                 where
                     width = length $ head inp
                     height = length inp
                     pointList = zip [ (x,y) | y <- [0 .. height - 1], x <- [0 .. width - 1] ] (concat inp)  
                     (cucumberE,cucumberS) = foldr insertIntoMap (S.empty,S.empty) pointList
                     insertIntoMap ((x,y),val) (setE,setS) | val == '.' = (setE,setS)
                                                           | val == '>' = (S.insert (x,y) setE, setS)
                                                           | val == 'v' = (setE, S.insert (x,y) setS)
                                                           | otherwise = error $ show ((x,y),val)

takeStepsTillStop :: Int -> (Int,Int) -> (CucumberSet,CucumberSet) -> (Int,(CucumberSet,CucumberSet))
takeStepsTillStop step dim sets | moved = takeStepsTillStop (step + 1) dim sets' 
                                | otherwise = (step,sets)
                                  where
                                      (moved,sets') = takeStep dim sets

takeStep :: (Int,Int) -> (CucumberSet,CucumberSet) -> (Bool,(CucumberSet,CucumberSet))
takeStep (w,h) (cSetE,cSetS) = (eastMove || southMove,(cSetE',cSetS'))
                               where
                                   (eastMove,cSetE') = takeStepE w (cSetE,cSetS)
                                   (southMove,cSetS') = takeStepS h (cSetE',cSetS)

takeStepE :: Int -> (CucumberSet,CucumberSet) -> (Bool,CucumberSet)
takeStepE w (cSetE,cSetS) = S.foldr moveEast (False,S.empty) cSetE
                            where
                                blocked (x,y) = S.member ((x + 1) `mod` w, y) cSetE || S.member ((x + 1) `mod` w,y) cSetS
                                moveEast (x,y) (b,newSet) | blocked (x,y) = (b,S.insert (x,y) newSet)
                                                              | otherwise = (True,S.insert ((x + 1) `mod` w,y) newSet)

takeStepS :: Int -> (CucumberSet,CucumberSet) -> (Bool,CucumberSet)
takeStepS h (cSetE,cSetS) = S.foldr moveSouth (False,S.empty) cSetS
                            where
                                blocked (x,y) = S.member (x, (y + 1) `mod` h) cSetE || S.member (x,(y + 1) `mod` h) cSetS
                                moveSouth (x,y) (b,newSet) | blocked (x,y) = (b,S.insert (x,y) newSet)
                                                           | otherwise = (True,S.insert (x,(y + 1) `mod` h) newSet)
