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

data ListR a = ListR Int [a] deriving Show
rotN :: Int -> Int -> ListR a -> ListR a
rotN rl n (ListR p l) = ListR (p+n) $
  take rl $ drop n $ l ++ cycle l

rev :: Int -> Int -> ListR a -> ListR a
rev rl n (ListR p l) = let
  (h,t) = splitAt n (cycle l)
  in ListR p $ take rl $ reverse h ++ t

eval :: Int -> Int -> Int -> ListR Int -> ListR Int
eval rl skip len l =
  rotN rl (len + skip) $ rev rl len l

evalList :: Int -> (Int,ListR Int) -> [Int] -> (Int, ListR Int)
evalList rl (skip,o) = foldl' folder (skip,o)
  where
  folder :: (Int,ListR Int) -> Int -> (Int,ListR Int)
  folder (s,acc) i = (s+1, eval rl s i acc)

passes :: Int -> [Int] -> Int -> (Int, ListR Int)
passes rl i n = last $ take (n+1) $ iterate (`f` i) (0, ListR 0 [0..rl-1])
  where
  f = evalList rl

denseHash :: [Int] -> [Int]
denseHash [] = []
denseHash l = if length chunk < 16
  then error "wrong size"
  else foldl' xor 0 chunk : denseHash (drop 16 l)
  where
  chunk =  take 16 l

toL :: ListR a -> [a]
toL (ListR p l) = let (h,t) = splitAt (length l - (p `mod`length l)) l in t++h

knotHash :: [Int] -> [Int]
knotHash input = denseHash $ toL $ snd $ passes 256 (input++[17, 31, 73, 47, 23]) 64

hashSrt :: [Int] -> String
hashSrt = concatMap (printf "%02x")

testKey :: String
testKey = "stpzcrnm"
--testKey = "flqrgnkx"

rowHash :: Int -> String -> String
rowHash rowNumber key = hashSrt $ knotHash $ map ord $ key ++ "-" ++ show rowNumber

bitRange :: Bits a => a -> Int -> Int -> [Bool]
bitRange n lo hi = reverse $ map (testBit n) [lo..hi]

bits :: Bits a => a -> [Bool]
bits n = bitRange n 0 (fromJust (bitSizeMaybe n) - 1)

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
