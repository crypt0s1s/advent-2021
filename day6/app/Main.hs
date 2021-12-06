module Main where

import Lib
import Data.List.Split
import qualified Data.Map as Map
import Debug.Trace

main :: IO ()
main = do
    inp <- readFile "day6.txt"
    print $ fishAtDayN (createFishMap $ parseInput inp) 256

type Fish = Int
type DaysLeft = Int
type Count = Int

parseInput :: String -> [Fish]
parseInput xs = map read (splitOn "," xs)

fishAtDayN :: Map.Map Fish Count -> Int -> Int
fishAtDayN fishCounts n = Map.foldrWithKey (calculateTotalFishAdd n) 0 (traceShowId fishCounts)

calculateTotalFishAdd :: Int -> Fish -> Count -> (Int -> Int)
calculateTotalFishAdd n fish count = (+) (calculateTotalFish n fish count) 

calculateTotalFish :: Int -> Fish -> Count -> Int
calculateTotalFish n fish count = count' * count
                                  where (Just count') = Map.lookup (n + 8 - fish) fishProducedByDaysTillEnd

createFishMap  :: [Fish] -> Map.Map Fish Count
createFishMap = foldl insertFish Map.empty
                where insertFish map' f = Map.insertWith (+) f 1 map'


fishProducedByDaysTillEnd :: Map.Map DaysLeft Count
fishProducedByDaysTillEnd = foldl fishProducedByDaysTillEnd' Map.empty daysLeft
                            where daysLeft = [0 .. 266]


fishProducedByDaysTillEnd' :: Map.Map DaysLeft Count -> DaysLeft -> Map.Map DaysLeft Count
fishProducedByDaysTillEnd' map' daysLeft = Map.insert daysLeft count map'
                                           where
                                               daysOffspringBecomeAdult = enumFromThenTo (daysLeft - 9) (daysLeft - 9 - 7) 0
                                               count = 1 + foldr (\x -> (+) (Map.findWithDefault 0 x map')) 0 daysOffspringBecomeAdult

