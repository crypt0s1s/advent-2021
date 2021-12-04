module Main where

import Lib
import Data.List.Split
import Debug.Trace
import Data.List

newtype Board = Board [[Int]]

instance Show Board where
    show (Board xs) = "Board:\n" ++ concatMap (\x -> show x ++ "\n") xs

data Status = Updated | Bingo | NoAction

main :: IO ()
main = do
    inp <- lines <$> readFile "day4.txt"
    print $ p2 $ parseIntput inp

genDraws :: String -> [Int]
genDraws xs = map read (splitOn "," xs)

genBoards :: [String] -> [Board]
genBoards xs = map genBoard chunks
               where
                 chunks = chunksOf 6 xs

genBoard :: [String] -> Board
genBoard xs = Board $ map genBoardLine (drop 1 xs)


genBoardLine :: String -> [Int]
genBoardLine xs = map read (words xs)

parseIntput :: [String] -> ([Int],[Board])
parseIntput (l:ls) = (draws,boards)
                      where draws = traceShowId $ genDraws l
                            boards = traceShowId $ genBoards ls


p1 :: ([Int],[Board]) -> Int
p1 (draw:draws, boards) = p1' (processDraw boards draw) draws
p1 ([], boards) = error "empty list reached"


p1' :: (Maybe Int, [Board]) -> [Int] -> Int
p1' (Just t, _) _ = t
p1' (Nothing, boards) draws = p1 (draws, boards) 

processDraw :: [Board] -> Int -> (Maybe Int,[Board])
processDraw boards draw = foldr (processDraw' draw) (Nothing, []) boards

-- for a draw go through a board and update check them
-- if a bingo has been found then return final result
processDraw' :: Int -> Board -> (Maybe Int, [Board]) -> (Maybe Int,[Board])
processDraw' draw _ (Just t, _) = (Just t,[])
processDraw' draw board (n, boards) = (processDraw'' r draw, b:boards)
                                     where (r,b) = searchUpdateBoard draw board

processDraw'' :: Maybe Int -> Int -> Maybe Int
processDraw'' (Just x) draw = Just (x * draw)
processDraw'' Nothing draw = Nothing

searchUpdateBoard :: Int -> Board -> (Maybe Int, Board)
searchUpdateBoard n (Board lines) | isUpdated = (maybeBoardSum, board')
                                  | otherwise = (Nothing, board')
                                    where updatedLines = map (searchUpdateCheckLine n) lines
                                          isUpdated = any fst updatedLines
                                          board' = Board (map snd updatedLines)
                                          maybeBoardSum = checkBoard board'

searchUpdateCheckLine :: Int -> [Int] -> (Bool,[Int])
searchUpdateCheckLine n line = (status,line')
                              where (status,line') = foldr (searchUpdateCheckLine' n) (False,[]) line

searchUpdateCheckLine' :: Int -> Int -> (Bool, [Int]) -> (Bool, [Int])
searchUpdateCheckLine' n x (True, xs) = (True, x:xs)
searchUpdateCheckLine' n x (False, xs) | x == n = (True, -1:xs)
                                       | otherwise = (False, x:xs)

checkBoard :: Board -> Maybe Int
checkBoard (Board lines) | horizontalLines || verticalLines = Just (countTotalLeft (Board lines))
                         | otherwise = Nothing
                         where
                             horizontalLines = any (all ((-1) ==)) lines
                             verticalLines = any (all ((-1) ==)) (transpose lines)

countTotalLeft :: Board -> Int
countTotalLeft (Board lines) = foldr sumBoardLine 0 lines

sumBoardLine :: [Int] -> (Int -> Int)
sumBoardLine xs = (+) (foldr isNegThen0 0 xs)

isNegThen0 :: Int -> (Int -> Int)
isNegThen0 x | x < 0 = (+) 0
             | otherwise = (+) x


-- part 2

p2 :: ([Int],[Board]) -> Int
p2 (draw:draws, boards) = p2' (processDraw2 boards draw) draws

p2' :: (Maybe Int, [Board]) -> [Int] -> Int 
p2' (Just t, []) _ = t
p2' (_, boards) draws = p2 (draws, boards)


processDraw2 :: [Board] -> Int -> (Maybe Int, [Board])
processDraw2 boards draw = foldr (processDraw2' draw) (Nothing, []) boards

processDraw2' :: Int -> Board -> (Maybe Int, [Board]) -> (Maybe Int, [Board])
processDraw2' draw board (n, boards) = (f, b' ++ boards)
                                      where (r,b) = searchUpdateBoard draw board
                                            (f,b') = processDraw2'' r draw b

processDraw2'' :: Maybe Int -> Int -> Board -> (Maybe Int, [Board])
processDraw2'' (Just x) draw board = (Just (x * draw), [])
processDraw2'' Nothing  _ board = (Nothing, [board])
