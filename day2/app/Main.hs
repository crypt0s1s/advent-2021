module Main where

import Lib
import Debug.Trace

main :: IO Int
main = do
    inp <- lines <$> readFile "day2.txt"
    return $ p2 inp

data Instruction = Forward Int
                 | Down Int
                 | Up Int

data State = State {
  aim :: Int,
  horizontal :: Int,
  depth :: Int
}

instance Show State where
    show st = "Aim: " ++ show (aim st) ++ ", Horizontal: " ++ show (horizontal st) ++ ", Depth: " ++ show (depth st)

instance Show Instruction where
    show (Forward n) = "Forward " ++ show n
    show (Down n) = "Down " ++ show n 
    show (Up n) = "Up " ++ show n

p1 :: [String] -> Int
p1 xs = final $ foldr splitStr (0,0,0) xs

final :: (Int,Int,Int) -> Int
final (a,b,c) = a * (b - c)

splitStr :: String -> (Int,Int,Int) -> (Int,Int,Int)
splitStr xs = process (head l, read $ l !! 1)
               where l = words xs

process :: (String,Int) -> (Int,Int,Int) -> (Int,Int,Int)
process ("forward", n) (a,b,c) = (a + n,b,c)
process ("down", n) (a,b,c) = (a,b+n,c)
process ("up", n) (a,b,c) = (a,b,c+n)
process _ _ = error "Not a proper instruction"

p2 :: [String] -> Int
p2 xs = horizontal st * depth st
        where processed = map splitStr' xs
              st = foldl updateState emptyState processed

splitStr' :: String -> Instruction
splitStr' xs = process' (head l, read $ l !! 1)
               where l = words xs

emptyState :: State
emptyState = State {
    aim = 0,
    horizontal = 0,
    depth = 0
}

updateState :: State -> Instruction -> State
updateState st (Forward n) = st { horizontal = horizontal st + n, depth = depth st + aim st * n }
updateState st (Down n)    = st { aim = aim st + n }
updateState st (Up n)      = st { aim = aim st - n}


process' :: (String, Int) -> Instruction
process' ("forward",n) = Forward n
process' ("down",n) = Down n
process' ("up",n) = Up n
process' _ = error "No matching instruction"