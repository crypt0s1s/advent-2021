module Main where

import Lib
import qualified Data.Map.Strict as Map
import Data.Maybe

type InsertionMap = Map.Map (Element,Element) Element
type PairCountsMap = Map.Map (Element,Element) Int
type CharCountMap = Map.Map Char Int
type Element = Char

main :: IO ()
main = do
         inp <- lines <$> readFile "day14.txt"
         let insertionMap = createInsertionMap $ drop 2 inp
         print $ p1 (head inp) insertionMap
         print $ p2 (head inp) insertionMap

p1 :: String -> InsertionMap -> ((Char,Int),(Char,Int))
p1 ls iMap = findMinMax $ Map.toList charCountsFinal
             where 
                zippedL = zip ls $ drop 1 ls
                pairCountsMapInitial = foldr (insertWithPlus 1) Map.empty zippedL
                charCountMapInitial = foldr (insertWithPlus 1) Map.empty ls
                charCountsFinal = takeNSteps 10 iMap pairCountsMapInitial charCountMapInitial

p2 :: String -> InsertionMap -> ((Char,Int),(Char,Int))
p2 ls iMap = findMinMax $ Map.toList charCountsFinal
             where 
                zippedL = zip ls $ drop 1 ls
                pairCountsMapInitial = foldr (insertWithPlus 1) Map.empty zippedL
                charCountMapInitial = foldr (insertWithPlus 1) Map.empty ls
                charCountsFinal = takeNSteps 40 iMap pairCountsMapInitial charCountMapInitial

insertWithPlus :: (Ord k, Num a) => a -> k -> Map.Map k a -> Map.Map k a
insertWithPlus n key pcMap = Map.insertWith (+) key n pcMap

getCharCounts :: PairCountsMap -> CharCountMap
getCharCounts pcMap = Map.foldrWithKey updateCharMap Map.empty pcMap
                      where
                          updateCharMap (a,b) v m = insertWithPlus v a (insertWithPlus v b m)

findMinMax :: [(Char,Int)] -> ((Char,Int),(Char,Int))
findMinMax (l:ls) = foldr findMinMax' (l,l) ls

findMinMax' :: (Char,Int) -> ((Char,Int),(Char,Int)) -> ((Char,Int),(Char,Int))
findMinMax' p (pMax,pMin) = (getMax p pMax, getMin p pMin)

getMax :: (Char,Int) -> (Char,Int) -> (Char,Int)
getMax (c,n) (c',n') | n > n' = (c,n)
                     | otherwise = (c',n')

getMin :: (Char,Int) -> (Char,Int) -> (Char,Int)
getMin (c,n) (c',n') | n < n' = (c,n)
                     | otherwise = (c',n')

createInsertionMap :: [String] -> InsertionMap
createInsertionMap lines = Map.fromList $ map (\x -> ((head (x !! 0), last (x !! 0)),head (x !! 2))) w
                           where 
                               w = map words lines

takeStep :: InsertionMap -> PairCountsMap -> CharCountMap -> (PairCountsMap,CharCountMap)
takeStep iMap pcMap ccMap = Map.foldrWithKey (takeStep' iMap) (Map.empty,ccMap) pcMap

takeStep' :: InsertionMap -> (Element,Element) -> Int -> (PairCountsMap,CharCountMap) -> (PairCountsMap,CharCountMap)
takeStep' iMap (a,b) x (pcMap,ccMap) | isNothing c = (insertWithPlus x (a,b) pcMap, ccMap)
                                     | otherwise = (insertWithPlus x (a,c') (insertWithPlus x (c',b) pcMap), ccMap')
                                       where
                                           c = iMap Map.!? (a,b)
                                           ccMap' = insertWithPlus x c' ccMap
                                           (Just c') = c

takeNSteps :: Int -> InsertionMap -> PairCountsMap -> CharCountMap -> CharCountMap
takeNSteps 0 _ _ ccMap  = ccMap
takeNSteps n iMap pcMap ccMap = takeNSteps (n - 1) iMap pcMap' ccMap'
                                where
                                    (pcMap',ccMap') = takeStep iMap pcMap ccMap
