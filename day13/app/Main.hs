module Main where

import Lib
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace

type Point = (Int,Int)
type Grid = Set.Set Point
type Instruction = String

main :: IO ()
main = do
    inpTest <- readFile "day13Test.txt"
    inp <- readFile "day13.txt"
    let (g',ix') = parseInput inpTest
    let (g,ix) = parseInput inp
    print $ Set.size $ p1 g' ix'
    putStr "\n\n"
    putStr $ showGrid $ p2 g' ix'
    putStr "\n\n"
    print $ Set.size $ p1 g ix
    putStr "\n\n"
    putStr $ showGrid $ p2 g ix

p1 :: Grid -> [Instruction] -> Grid
p1 g ix = foldPaper g (head ix)

p2 :: Grid -> [Instruction] -> Grid
p2 g ix = foldl foldPaper g ix

parseInput :: String -> (Grid,[Instruction])
parseInput xs = (createGrid points, parseInstrux instrux) 
                where
                   [points,instrux] = splitOn "\n\n" xs

createGrid :: String -> Grid
createGrid xs = Set.fromList $ map lineToPoint ls
                where
                   ls = lines xs

lineToPoint :: String -> Point
lineToPoint xs = (read x, read y)
                 where
                     [x,y] = splitOn "," xs

parseInstrux :: String -> [Instruction]
parseInstrux xs = map (\l -> words l !! 2) ls
                  where
                      ls = lines xs

showGrid :: Grid -> String
showGrid g = concatMap (\(xs,y) -> showGrid' xs y g) $ zip (replicate (mxY + 1) [mnX .. mxX]) [mnY .. mxY]
    where
        (mnX,mnY,mxX,mxY) = findMinMax g

showGrid' :: [Int] -> Int -> Grid -> String
showGrid' [] _ _ = "\n"
showGrid' (x:xs) y g | Set.member (x,y) g = '#' : showGrid' xs y g
                     | otherwise = '.' : showGrid' xs y g

findMinMax :: Grid -> (Int,Int,Int,Int)
findMinMax = Set.foldr (\(x,y) (mnX,mnY,mxX,mxY) -> (min x mnX, min y mnY, max x mxX, max y mxY)) (1000,1000,0,0)

foldPaper :: Grid -> Instruction -> Grid
foldPaper g ('x':'=':xs) = foldGridX g (read xs)
foldPaper g ('y':'=':ys) = foldGridY g (read ys)

foldGridX :: Grid -> Int -> Grid
foldGridX g fx = Set.foldr (\(x,y) set -> Set.insert (2 * fx - x,y) set) leftFold rightFold
                where
                    (leftFold,rightFold) = Set.partition (\(x,y) -> x < fx) g

foldGridY :: Grid -> Int -> Grid
foldGridY g fy = Set.foldr (\(x,y) set -> Set.insert (x,2 * fy - y) set) topFold bottomFold
                where
                    (topFold,bottomFold) = Set.partition (\(x,y) -> y < fy) g
