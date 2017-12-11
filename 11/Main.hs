module Main where

import Data.List
import Data.Function (on)

data Vect = Vect Integer Integer deriving Show

_x :: Vect -> Integer
_x (Vect x _) = x

_y :: Vect -> Integer
_y (Vect _ y) = y

vPlus :: Vect -> Vect -> Vect
vPlus (Vect a b) (Vect c d) = Vect (a+c) (d+b)

data Dir = N | NW | SW | S | SE | NE deriving Show

parseDir :: String -> Either String Dir
parseDir "n" = Right N
parseDir "nw" = Right NW
parseDir "sw" = Right SW
parseDir "s" = Right S
parseDir "se" = Right SE
parseDir "ne" = Right NE
parseDir s = Left $ "Unknown direction: " ++ s

dirVec :: Dir -> Vect
dirVec N = Vect 0 (-1)
dirVec NW = Vect (-1) 0
dirVec SW = Vect (-1) 1
dirVec S = Vect 0 1
dirVec SE = Vect 1 0
dirVec NE = Vect 1 (-1)

type Pos = Vect

move :: Pos -> Dir -> Pos
move p d = dirVec d `vPlus` p

hexDistance :: Pos -> Pos -> Integer
hexDistance a b =
  (
    abs(on (-) _x a b) +
    abs(on (-) _y a b) +
    abs(_x a + _y a - _x b - _y b)
  ) `div` 2

main :: IO ()
main =
  parseInput <$> readFile "input.txt" >>=
  --print . (hexDistance (Vect 0 0) . foldl' move (Vect 0 0) <$>)
  print . (maximum . snd . mapAccumL mapper (Vect 0 0) <$>)
  where 
  mapper :: Pos -> Dir -> (Pos, Integer)
  mapper p d = let
    np = move p d in (np, hexDistance (Vect 0 0) np)

parseInput :: String -> Either String [Dir]
parseInput = mapM parseDir . words . map (\x -> if x == ',' then ' ' else x)
