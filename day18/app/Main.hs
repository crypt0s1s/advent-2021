module Main where

import Lib ()
import Data.List.Split
import Data.Char (digitToInt)
import Data.Either
import Data.Maybe
import Debug.Trace

data SnailMath = Lit Int | Pair Int SnailMath SnailMath

instance Show SnailMath where
    show (Lit i) = show i
    show (Pair d p1 p2) = "[ depth: " ++ show d ++ ", " ++ show p1 ++ "," ++ show p2 ++ " ]"

main :: IO ()
main = do
         inpTest <- lines <$> readFile "day18.txt"
         let m = map (toSnailMath . toNumDepthTup 0) inpTest
         print $ calcMagnitude $ p1 m
         print $ p2 m

p1 :: [SnailMath] -> SnailMath
p1 (sm:sms) = foldl addSnailMath sm sms

p2 :: [SnailMath] -> Int
p2 sms = maximum [calcMagnitude (addSnailMath s s') | (s,n) <- sms', (s',n') <- sms', n /= n']
         where
             sms' = zip sms [0..]

toSnailMath :: [(Int,Int)] -> SnailMath
toSnailMath ls = toSnailMath' (map Left ls) mx
                 where mx = maximum $ map snd ls

toSnailMath' :: [Either (Int,Int) (SnailMath,Int)] -> Int -> SnailMath
toSnailMath' ls 0 = fst . head $ rights ls
toSnailMath' ls depth = toSnailMath' (toSnailMath'' ls depth) (depth - 1)

toSnailMath'' :: [Either (Int,Int) (SnailMath,Int)] -> Int -> [Either (Int,Int) (SnailMath,Int)]
toSnailMath'' (Left (v, d) : Left (v', d') : ls) depth | d == depth && d == d' = Right (Pair d (Lit v) (Lit v'), d - 1) : toSnailMath'' ls depth
                                                       | otherwise = Left (v, d) : toSnailMath'' (Left (v', d') : ls) depth
toSnailMath'' (Right (sm, d) : Left (v', d') : ls) depth | d == depth && d' == d = Right (Pair d sm (Lit v'), d - 1) : toSnailMath'' ls depth
                                                         | otherwise  = Right (sm, d) : toSnailMath'' (Left (v', d') : ls) depth
toSnailMath'' (Left (v, d) : Right (sm', d') : ls) depth | d == depth && d' == d = Right (Pair d (Lit v) sm', d - 1) : toSnailMath'' ls depth
                                                         | otherwise  = Left (v, d) : toSnailMath'' (Right (sm', d') : ls) depth
toSnailMath'' (Right (sm, d) : Right (sm', d') : ls) depth | d == depth && d' == d = Right (Pair d sm sm', d - 1) : toSnailMath'' ls depth
                                                           | otherwise = Right (sm,d) : toSnailMath'' (Right (sm', d') : ls) depth
toSnailMath'' (l : ls) depth = l : toSnailMath'' ls depth
toSnailMath'' [] _ = []

toNumDepthTup :: Int -> String -> [(Int,Int)]
toNumDepthTup d ('[':ls) = toNumDepthTup (d + 1) ls
toNumDepthTup d (']':ls) = toNumDepthTup (d - 1) ls
toNumDepthTup d (',':ls) = toNumDepthTup d ls
toNumDepthTup d (l:ls) = (digitToInt l,d) : toNumDepthTup d ls
toNumDepthTup _ [] = []

addSnailMath :: SnailMath -> SnailMath -> SnailMath
addSnailMath sm sm' = reduceSnailMath $ Pair 1 (incrementDepth sm) (incrementDepth sm')

addSnailMath' :: SnailMath -> SnailMath -> SnailMath
addSnailMath' sm sm' = Pair 1 (incrementDepth sm) (incrementDepth sm')

incrementDepth :: SnailMath -> SnailMath
incrementDepth (Lit n) = Lit n
incrementDepth (Pair d p1 p2) = Pair (d + 1) (incrementDepth p1) (incrementDepth p2)

reduceSnailMath :: SnailMath -> SnailMath
reduceSnailMath sm | contEx = reduceSnailMath $ explode sm
                   | canSplit sm = reduceSnailMath $ split' 1 sm
                   | otherwise = sm
                   where
                       (contEx,_,_) = containsExploding sm

split' :: Int -> SnailMath -> SnailMath
split' depth (Lit n) | n > 9 = Pair depth (Lit n1) (Lit n2)
                     | otherwise = Lit n
                     where
                         n1 = n `quot` 2
                         n2 = if n1 * 2 == n then n1 else n1 + 1
split' depth (Pair d p1 p2) | canSplit p1 = Pair d (split' (d+1) p1) p2
                            | otherwise = Pair d p1 (split' (d+1) p2)

canSplit :: SnailMath -> Bool
canSplit (Lit n) = n > 9
canSplit (Pair _ p1 p2) = canSplit p1 || canSplit p2

explode :: SnailMath -> SnailMath
explode sm | containsEx = explode sm'''
           | otherwise = sm
            where (containsEx,_,_) = containsExploding sm
                  sm' = snd $ searchLeft sm
                  sm'' = snd $ searchRight sm'
                  sm''' = resetExplosion sm''

containsExploding :: SnailMath -> (Bool, Maybe Int, Maybe Int)
containsExploding (Lit _) = (False, Nothing, Nothing)
containsExploding (Pair d p1 p2 ) | d > 4 = (True, Just l, Just r)
                                  | t1 = (True, l', r')
                                  | otherwise = containsExploding p2
                                  where
                                      (Lit l) = p1
                                      (Lit r) = p2
                                      (t1, l', r') = containsExploding p1

searchLeft :: SnailMath -> (Bool,SnailMath)
searchLeft (Pair d p1 p2) | conEx1 && s = (True, Pair d p1' p2)
                          | conEx1 = (False, Pair d p1 p2)
                          | conEx2 && s' = (True, Pair d p1 p2')
                          | conEx2 = (True, Pair d (updateLeft p1 (fromJust l)) p2)
                          | otherwise = (False, Pair d p1 p2)
                            where
                                (conEx1, _, _) = containsExploding p1
                                (conEx2, l, _) = containsExploding p2
                                (s,p1')  = searchLeft p1
                                (s',p2') = searchLeft p2
searchLeft v = (False, v)

updateLeft :: SnailMath -> Int -> SnailMath
updateLeft (Lit v) v' = Lit (v + v')
updateLeft (Pair d p1 p2) v' = Pair d p1 (updateLeft p2 v')

searchRight :: SnailMath -> (Bool,SnailMath)
searchRight (Pair d p1 p2) | conEx1 && s = (True, Pair d p1' p2)
                           | conEx1 = (True, Pair d p1 (updateRight p2 (fromJust r)))
                           | conEx2 && s' = (True, Pair d p1 p2')
                           | conEx2 = (False, Pair d p1 p2)
                           | otherwise = (False, Pair d p1 p2)
                             where
                                 (conEx1, _, r) = containsExploding p1
                                 (conEx2, _, _) = containsExploding p2
                                 (s, p1') = searchRight p1
                                 (s',p2') = searchRight p2
searchRight v = (False, v)

updateRight :: SnailMath -> Int -> SnailMath
updateRight (Lit v) v' = Lit (v + v')
updateRight (Pair d p1 p2) v' = Pair d (updateRight p1 v') p2

resetExplosion :: SnailMath -> SnailMath
resetExplosion (Lit n) = Lit n
resetExplosion (Pair 5 _ _) = Lit 0
resetExplosion (Pair d p1 p2) | conEx1 = Pair d (resetExplosion p1) p2
                              | conEx2 = Pair d p1 (resetExplosion p2)
                              | otherwise = Pair d p1 p2
                              where
                                  (conEx1,_,_) = containsExploding p1
                                  (conEx2,_,_) = containsExploding p2

calcMagnitude :: SnailMath -> Int
calcMagnitude (Lit i) = i
calcMagnitude (Pair d o1 o2) = calcMagnitude o1 * 3 + calcMagnitude o2 * 2