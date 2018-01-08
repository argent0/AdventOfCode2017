module Main where

import Control.Arrow
import Data.Function
import Data.List (foldl')

gen :: Integer -> Integer -> [Integer]
gen kernel = drop 1 . iterate f
  where
  f = (`rem` 2147483647) . (*kernel)

genA :: Integer -> [Integer]
genA = gen 16807

genB :: Integer -> [Integer]
genB = gen 48271

testInput :: (Integer, Integer)
testInput = (65, 8921)

myInput :: (Integer, Integer)
myInput = (703,516)


part1 :: (Integer, Integer) -> Integer
part1 (a, b)=
  foldl' (+) 0 $
    take 40000000 $
    map boolToInt $ zipWith zipper (genA a) (genB b)
  where
  boolToInt True = 1
  boolToInt False = 0
  zipper :: Integer -> Integer -> Bool
  zipper = (==) `on` (`rem` 2^16)

part2 :: (Integer, Integer) -> Integer
part2 (a, b)=
  foldl' (+) 0 $
    take 5000000 $
    map boolToInt $ zipWith zipper
      (filter ((==0) . (`mod` 4)) $ genA a)
      (filter ((==0) . (`mod` 8)) $ genB b)
  where
  boolToInt True = 1
  boolToInt False = 0
  zipper :: Integer -> Integer -> Bool
  zipper = (==) `on` (`rem` 2^16)

main :: IO ()
main =
  --print $ part1 myInput
  print $ part2 myInput
