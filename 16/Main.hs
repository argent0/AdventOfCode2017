{-# LANGUAGE Strict #-}

module Main where

import qualified Data.Array.IArray as IA
import Data.List
import Data.Maybe
import Debug.Trace (trace)
-- import Control.Monad.ST.Lazy
-- import Data.Array.ST
import qualified Data.Vector.Unboxed as UV

--type Dancers = IA.Array Int Char
type Dancers = UV.Vector Char

data Move
  = Spin Int
  | Exchange Int Int
  | Partner Char Char deriving Show

spin :: Int -> Dancers -> Dancers
spin n dancers =
  IA.listArray (minb, maxb) $ map (dancers IA.!) [maxb-n+1..maxb] ++ map (dancers IA.!) [minb..maxb-n]
  -- IA.listArray (minb, maxb) $ t ++ h --Slower
  where
  (h,t) = splitAt (maxb+1-n) e
  e = IA.elems dancers
  (minb, maxb) = IA.bounds dancers

exchange :: Int -> Int -> Dancers -> Dancers
exchange a b dancers =
  dancers IA.// [(a, dancers IA.! b), (b, dancers IA.! a)]

partner :: Char -> Char -> Dancers -> Dancers
partner a b dancers =
  dancers IA.// [(fst ea, snd eb), (fst eb, snd ea)]
  where
  ea = fromJust $ find ((== a) . snd) $ IA.assocs dancers
  eb = fromJust $ find ((== b) . snd) $ IA.assocs dancers

eval :: Move -> Dancers -> Dancers
eval (Spin n) = spin n
eval (Exchange a b) = exchange a b
eval (Partner a b) = partner a b

evalList :: Dancers -> [Move] -> Dancers
evalList = foldr eval

dancers :: Dancers
dancers = IA.listArray (0,15) ['a'..'p']

testDancers :: Dancers
testDancers = IA.listArray (0,4) ['a'..'e']

testInput :: String
testInput = "s1,x3/4,pe/b"

part1 :: Dancers -> String -> String
part1 dancers input =
  IA.elems $ evalList dancers (reverse $ parseInput input)

miterate :: Integer -> (a->a) -> a -> a
miterate n f a
  | n < 0 = error "Negative iterte" 
  | n == 0 = a
  | otherwise =
      (if n `mod` 1000 == 0
              then trace (show n)
              else id) miterate (n-1) f (f a)

part2 :: Integer -> Dancers -> String -> String
part2 n dancers input =
  IA.elems $ miterate n (`evalList` (reverse $ parseInput input)) dancers

main :: IO ()
main =
  --readFile "input.txt" >>= print . part1 dancers
  readFile "input.txt" >>= print . part2 1000 dancers

parseInput :: String -> [Move]
parseInput = map parseMove . words . map (\x -> if x == ',' then ' ' else x)

-- moveOptimize :: [Move] -> [Move]
-- moveOptimize [] = []
-- moveOptimize [m] = [m]
-- moveOptimize (m@(Spin n):n:ms) = 
-- moveOptimize (m@(Exchange _ _):n:ms) = m:(moveOptimize n:ms)
-- moveOptimize (m@(Partner _ _):n:ms) = m:(moveOptimize n:ms)

parseMove :: String -> Move
parseMove ('s':rest) = Spin $ read rest
parseMove ('x':rest) = Exchange (read a) (read b)
  where
  [a,b] = words $ map (\x -> if x == '/' then ' ' else x) rest
parseMove ('p':rest) = Partner a b
  where
  [[a],[b]] = words $ map (\x -> if x == '/' then ' ' else x) rest
