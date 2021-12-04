module Main where

import Lib

import Debug.Trace
import Data.Char
import Data.List

data Cat = OXY | CO2

main :: IO (Int,Int)
main = do
    inp <- lines <$> readFile "day3.txt"
    return $ p2 inp


p1 :: [String] -> Int
p1 xs = tg * te
        where l = length xs
              counts = foldr p1' (repeat 0) xs
              proccessCounts = map (proccessCount l) counts
              (_,tg,te) = foldr convertIntListToBin (1,0,0) proccessCounts

p1' :: String -> [Int] -> [Int]
p1' = zipWith p1''

p1'' :: Char -> Int -> Int
p1'' '1' n = n + 1
p1'' '0' n = n
p1'' _ _ = error "Not 1 or 0"

proccessCount :: Int -> Int -> Int
proccessCount l x | x > l `quot` 2 = 1
                  | otherwise = 0

convertIntListToBin :: Int -> (Int,Int,Int) -> (Int,Int,Int)
convertIntListToBin n (m,tg,te) = (m * 2, tg + m * n, te + m * n')
                                  where n' = if n == 1 then 0 else 1

p2 :: [String] -> (Int,Int)
p2 xs = (og,co)
       where
             sorted = sort xs
             (a,b) = partitionBin sorted ([],[])
             co = co2 a 1
             og = oxygen b 1

co2 :: [String] -> Int -> Int
co2 [x] _ = snd $ foldr (convertIntListToBin' . digitToInt) (1,0) x
co2 xs p | count0 > count1 = co2 (take count1 xs) (p + 1)
         | otherwise = co2 (drop count1 xs) (p + 1)
           where (count0,count1) = foldr (\x -> countBitSumAtPos $ x !! p) (0,0) xs

oxygen :: [String] -> Int -> Int
oxygen [x] _ = snd $ foldr (convertIntListToBin' . digitToInt) (1,0) x
oxygen xs p | count1 >= count0 = oxygen (drop count0 xs) (p + 1)
            | otherwise = oxygen (take count0 xs) (p + 1)
              where (count0,count1) = foldr (\x -> countBitSumAtPos $ x !! p) (0,0) xs

countBitSumAtPos :: Char -> (Int,Int) -> (Int,Int)
countBitSumAtPos '0' (a,b) = (a+1,b)
countBitSumAtPos '1' (a,b) = (a,b+1)
countBitSumAtPos _ _ = error "Not 1 or 0"

convertIntListToBin' :: Int -> (Int,Int) -> (Int,Int)
convertIntListToBin' n (m,tg) = (m * 2, tg + m * n)

-- String must be sorted
-- takes while string starts with 0 and puts it into the first pos in the tuple
-- puts remainder of string in 2nd pos
partitionBin :: [String] -> ([String],[String]) -> ([String],[String])
partitionBin (('0':ls):zs) (xs,[]) = partitionBin zs (('0':ls): xs, [])
partitionBin ls (xs,[]) = (xs,ls)
partitionBin _ _ = error "Error in partitionBin"


-- p2 iteration 1
{-
p2' :: [String] -> (Int,Int)
p2' xs = (og,co)
         where l = length xs
               counts = foldr p1' (repeat 0) xs
               og = filterDigits xs 0 OXY
               co = filterDigits xs 0 CO2

filterDigits :: [String] -> Int -> Cat -> Int
filterDigits [x] _ c = z
                 where (_,z,_) = foldr (convertIntListToBin . digitToInt) (1,0,0) x
filterDigits xs p c = filterDigits (filter condition xs) (p + 1) c
                 where condition x = digitToInt (x !! p) == countPos xs p c

countPos :: [String] -> Int -> Cat -> Int
countPos xs p OXY | foldr (\x -> p1'' $ x !! p) 0 xs * 2 >= length xs = 1
                  | otherwise = 0
countPos xs p CO2 | foldr (\x -> p1'' $ x !! p) 0 xs * 2 >= length xs = 0
                  | otherwise = 1
-}


-- P2 iteration 2
{-

p2''' :: [String] -> (Int,Int)
p2''' xs = (og,co)
          where l = length xs
                counts = foldr p1' (repeat 0) xs
                og = filterDigits' xs 0 OXY
                co = filterDigits' xs 0 CO2


p2'' :: Char -> (Int,Int) -> (Int,Int)
p2'' '1' (a,b) = (a+1,b)
p2'' '0' (a,b) = (a,b+1)
p2'' _ _ = error "Not 1 or 0"

filterDigits' :: [String] -> Int -> Cat -> Int
filterDigits' [x] _ c = z
                 where (_,z,_) = foldr (convertIntListToBin . digitToInt) (1,0,0) x
filterDigits' xs p c = filterDigits' (filter condition xs) (p + 1) c
                 where condition x = digitToInt (x !! p) == countPos' xs p c

countPos' :: [String] -> Int -> Cat -> Int
countPos' xs p OXY | a >= b = 1
                   | otherwise = 0
                   where (a,b) = foldr (\x -> p2'' $ x !! p) (0,0) xs
countPos' xs p CO2 | b >= a = 0
                   | otherwise = 1
                   where (a,b) = foldr (\x -> p2'' $ x !! p) (0,0) xs

-}