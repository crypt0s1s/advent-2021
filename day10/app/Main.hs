module Main where

import Lib
import Data.Maybe
import Debug.Trace
import Data.List ( sort )

main :: IO ()
main = do
    inpTest <- lines <$> readFile "day10Test.txt"
    inp <- lines <$> readFile "day10.txt"
    print $ part1 inpTest
    print $ part1 inp
    print $ part2 inpTest
    print $ part2 inp

part1 :: [String] -> Int
part1 ls = decodeAll $ mapMaybe (fst . isValidChunk []) ls

part2 :: [String] -> Int
part2 ls = sort autocompleteScores !! (length autocompleteScores `div` 2)
           where
               chunkValidity = map (isValidChunk []) ls
               incompleteChunks = filter (isNothing . fst) chunkValidity
               autocompleteScores = map (autocompleteScore . snd) incompleteChunks
               

autocompleteScore :: String -> Int
autocompleteScore = foldl (\t x -> (t * 5) + decode' x) 0

isValidChunk :: String -> String -> (Maybe Char,String)
isValidChunk stack [] = (Nothing,stack)
isValidChunk stack (x:xs) | isOpen x = isValidChunk (inverse x:stack) xs
                          | x == head stack = isValidChunk (tail stack) xs
                          | otherwise = (Just x,stack)

-- returns the close bracket for any open bracket
inverse :: Char -> Char
inverse '(' = ')'
inverse '{' = '}'
inverse '[' = ']'
inverse '<' = '>'
inverse _ = error "Not open bracked"

-- is open bracked
isOpen :: Char -> Bool
isOpen c = c `elem` "([{<"

decodeAll :: [Char] -> Int
decodeAll = foldr (\x y -> y + decode x) 0

-- used to decode chars in p1
decode :: Char -> Int
decode ')' = 3
decode ']' = 57
decode '}' = 1197
decode '>' = 25137
decode _ = error "Unexpected value in decde"

-- used to decode chars in p2
decode' :: Char -> Int
decode' ')' = 1
decode' ']' = 2
decode' '}' = 3
decode' '>' = 4
decode' _ = error "Unexpected value in decde"