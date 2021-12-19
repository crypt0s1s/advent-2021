module Main where

import Lib
-- import qualified Data.ByteString as BS
import Data.Char (digitToInt)
import Data.List.Split (chunksOf)
import Debug.Trace

type Type = Int
type Version = Int
type Value = Int
data Packet = Operator Version Type Value [Packet]
            | Literal  Version Type Value

instance Show Packet where
    show (Operator v t val ps) = "Operator Version: " ++ show v ++ ", Type: " ++ show t ++ ", Value: " ++ show val ++ ", Sub-Packets:\n" ++ show ps ++ "\n"
    show (Literal v t val) = "Literal Version: " ++ show v ++ ", Type: " ++ show t ++ ", Value: " ++ show val ++ "\n"

main :: IO ()
main = do
    inp <- readFile "day16.txt"
    let (packet,_) = parsePacket $ decode inp
    print $ p1 packet
    print $ p2 packet

decode :: String -> String
decode = concatMap decodeChar

decodeChar :: Char -> String
decodeChar '0' = "0000"
decodeChar '1' = "0001"
decodeChar '2' = "0010"
decodeChar '3' = "0011"
decodeChar '4' = "0100"
decodeChar '5' = "0101"
decodeChar '6' = "0110"
decodeChar '7' = "0111"
decodeChar '8' = "1000"
decodeChar '9' = "1001"
decodeChar 'A' = "1010"
decodeChar 'B' = "1011"
decodeChar 'C' = "1100"
decodeChar 'D' = "1101"
decodeChar 'E' = "1110"
decodeChar 'F' = "1111"
decodeChar _ = error "Not a hexadecimal value"

addLeading0s :: String -> String
addLeading0s xs | r == 0 = xs
                | otherwise = replicate r '0' ++ xs
                  where r = length xs `mod` 4

parsePacket :: String -> (Packet,String)
parsePacket xs | t == 4 = (Literal v t (fromBits litBits),otherPackets)
               | otherwise = (Operator v t opPacketValue opSubPackets,otherPackets')
                 where
                     (header,body) = splitAt 6 xs
                     (v,t) = getVersionType header
                     (litBits,otherPackets) = calcLiteral body
                     (opSubPackets,otherPackets') = parseOpPacket t body
                     opPacketValue = findPacketValue t opSubPackets

parseOpPacket :: Type -> String -> ([Packet],String)
parseOpPacket t (lt:body) | lt == '0' = (parsedSubPackets,otherPackets)
                          | lt == '1' = parseNPackets noSubPackets body''
                          | otherwise = error "Unknown Bit Value"
                          where
                              (lengthBits,body') = splitAt 15 body
                              lengthBody = fromBits lengthBits
                              (subPackets,otherPackets) = splitAt lengthBody body'
                              (parsedSubPackets,_) = parseNPackets lengthBody subPackets
                              
                              (subPacketsBits, body'') = splitAt 11 body
                              noSubPackets = fromBits subPacketsBits
parseOpPacket _ body = ([],body)

parseNPackets :: Int -> String -> ([Packet],String)
parseNPackets _ [] = ([],[])
parseNPackets 0 packets = ([],packets)
parseNPackets n packets = (packet:otherParsedPackets,remainder)
                          where
                              (packet, otherPackets) = parsePacket packets
                              (otherParsedPackets,remainder) = parseNPackets (n - 1) otherPackets

getVersionType :: String -> (Version, Type)
getVersionType xs = (fromBits v, fromBits t)
                    where
                        (v, t) = splitAt 3 xs

fromBits :: String -> Int
fromBits = foldl (\t x -> t * 2 + digitToInt x) 0

calcLiteral :: String -> (String,String)
calcLiteral ('1':xs) = (a ++ rest, otherPackets)
                       where
                           (a,b) = splitAt 4 xs
                           (rest,otherPackets) = calcLiteral b
calcLiteral ('0':xs) = splitAt 4 xs

p1 :: Packet -> Int
p1 (Operator v _ _ ps) = v + sum (map p1 ps)
p1 (Literal v _ _) = v

p2 :: Packet -> Int 
p2 (Operator _ _ val _) = val 
p2 (Literal _ _ val) = val

findPacketValue :: Type -> [Packet] -> Int
findPacketValue t packets | t == 0 = sum values
                          | t == 1 = product values
                          | t == 2 = minimum values
                          | t == 3 = maximum values
                          | t == 5 = if head values >  last values then 1 else 0
                          | t == 6 = if head values <  last values then 1 else 0
                          | t == 7 = if head values == last values then 1 else 0
                          | otherwise = error "Not a valid operator type"
                          where values = map getPacketVal packets

getPacketVal :: Packet -> Value
getPacketVal (Literal _ _ val) = val
getPacketVal (Operator _ _ val _) = val
