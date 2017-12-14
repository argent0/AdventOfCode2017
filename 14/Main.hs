{-# LANGUAGE TupleSections #-}
module Main where

import Data.List (foldl')
import Data.Char (ord)
import Data.Bits
import Text.Printf
import Data.Maybe
import Data.Word (Word8)
import Numeric
import Data.Graph as G
import qualified Data.Array.IArray as IA

--------------------------------------------------------------------------------
-- A cyclic list that keeps a current position
-- It supports the operations rotN and rev

data ListR a = ListR
  Int --list length
  Int --list current position
  [a] --list content
  deriving Show

listRlength :: ListR a -> Int
listRlength (ListR l _ _) = l

rotN :: Int -> ListR a -> ListR a
rotN n (ListR rl p l) = ListR rl (p+n) $
  take rl $ drop n $ l ++ cycle l

rev :: Int -> ListR a -> ListR a
rev n (ListR rl p l) = let
  (h,t) = splitAt n (cycle l)
  in ListR rl p $ take rl $ reverse h ++ t

toL :: ListR a -> [a]
toL (ListR rl p l) = let (h,t) = splitAt (rl - (p `mod` rl)) l in t++h

--------------------------------------------------------------------------------
-- Knot hashes

-- Evalutates a jump length
eval :: Int -> Int -> ListR Int -> ListR Int
eval skip len l =
  rotN (len + skip) $ rev len l

-- Evalutates a list of jumps
evalList :: (Int,ListR Int) -> [Int] -> (Int, ListR Int)
evalList (skip,o) = foldl' folder (skip,o)
  where
  folder :: (Int,ListR Int) -> Int -> (Int,ListR Int)
  folder (s,acc) i = (s+1, eval s i acc)

-- Does the passes
passes :: Int -> [Int] -> Int -> (Int, ListR Int)
passes rl i n = last $ take (n+1) $ iterate (`evalList` i) (0, ListR rl 0 [0..rl-1])

denseHash :: [Int] -> [Int]
denseHash [] = []
denseHash l = if length chunk < 16
  then error "wrong size"
  else foldl' xor 0 chunk : denseHash (drop 16 l)
  where
  chunk =  take 16 l

knotHash :: [Int] -> [Int]
knotHash input = denseHash $ toL $ snd $ passes 256 (input++[17, 31, 73, 47, 23]) 64

hashSrt :: [Int] -> String
hashSrt = concatMap (printf "%02x")

--------------------------------------------------------------------------------
-- Bit helpers
bitRange :: Bits a => a -> Int -> Int -> [Bool]
bitRange n lo hi = reverse $ map (testBit n) [lo..hi]

bits :: Bits a => a -> [Bool]
bits n = bitRange n 0 (fromJust (bitSizeMaybe n) - 1)

--------------------------------------------------------------------------------
-- Day 14

testKey :: String
testKey = "stpzcrnm"
--testKey = "flqrgnkx"

rowHash :: Int -> String -> String
rowHash rowNumber key = hashSrt $ knotHash $ map ord $ key ++ "-" ++ show rowNumber

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

-- raises error if Char is not valid hex
hexBits :: Char -> [Bool]
hexBits a = drop 4 $ bits h
  where
  h :: Word8
  h = fst $ head $ readHex [a]

part1 :: String -> Int
part1 key = sum rowSums
  where
  rowSums = map (sum . map boolToInt) rowBits
  rowBits = map (concatMap hexBits) rows
  rows = map (`rowHash` key) [0..127]

adj :: Int -> [Int]
adj col
  | col < 0 = error "Negative"
  | col == 0 = [1]
  | col == 127 = [126]
  | otherwise = [col-1, col+1]

adjwS :: Int -> Int -> [Int]
adjwS size col
  | col < 0 = error "Negative"
  | col == 0 = [1]
  | col == size-1 = [size-2]
  | otherwise = [col-1, col+1]

type Cell = (Int,Int)
adjCells :: Cell -> [Cell]
adjCells c@(row, col) = c:(map (,col) (adj row) ++ map (row,) (adj col))

adjCellswS :: Int -> Cell -> [Cell]
adjCellswS size c@(row, col) = c:(map (,col) (adjwS size row) ++ map (row,) (adjwS size col))

type Disk = IA.Array Cell Bool
diskAdjCells :: Disk -> Cell -> [Cell]
diskAdjCells disk pos =
  if disk IA.! pos
    then filter f $ adjCells pos
    else []
  where
  f :: Cell -> Bool
  f = (disk IA.!)

diskAdjCellswS :: Int -> Disk -> Cell -> [Cell]
diskAdjCellswS size disk pos =
  if disk IA.! pos
    then filter f $ adjCellswS size pos
    else []
  where
  f :: Cell -> Bool
  f = (disk IA.!)

part2 :: String -> Int
part2 key = length $ components gr
  where
  (gr,_,_) = G.graphFromEdges $
    filter (\(_,_,l) -> not $ null l) $
    map mkNod $ (,) <$> [0..size] <*> [0..size]
  mkNod :: Cell -> (Cell, Cell, [Cell])
  mkNod cell = (cell, cell, diskAdjCells disk cell)
  disk :: Disk
  disk = IA.array ((0,0),(size,size)) $ zip ((,) <$> [0..size] <*> [0..size]) $ concat rowBits
  rowBits = map (concatMap hexBits) rows
  rows = map (`rowHash` key) [0..size]
  size :: Int
  size = 127

main :: IO ()
main = print $ part2 testKey
