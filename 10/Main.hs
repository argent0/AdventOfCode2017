{-# LANGUAGE Strict #-}
module Main where

import Control.Arrow
import Data.List (foldl')
import Data.Char (ord)
import Data.Bits
import Text.Printf

import Debug.Trace (trace)

data ListR a = ListR [a] a [a] deriving Show

toL :: ListR a -> [a]
toL (ListR f a t) = f ++ (a:t)

rev :: Show a => Int -> ListR a -> ListR a
rev n l@(ListR f a t) = -- trace (show res) $
  uncurry (ListR (drop (length t + 1) res)) $ first head $ splitAt 1 $ take (length t + 1) res
  where
  res = uncurry (++) $ first reverse $ splitAt n ((a:t) ++ f)

rot :: ListR a -> ListR a
rot (ListR [] a []) = ListR [] a []
rot (ListR (x:xs) a []) = ListR [] x (xs++[a])
rot (ListR [] a (y:ys)) = ListR [a] y ys
rot (ListR xs a (y:ys)) = ListR (xs++[a]) y ys

rotN :: Int -> ListR a -> ListR a
rotN 0 l = l
rotN n l = rotN (n-1) (rot l)

eval :: Show a => (Int, ListR a) -> Int -> (Int, ListR a)
eval (skip,l) len = --trace (show ((skip,l),len)) $
  (skip + 1, rotN (len + skip) $ rev len l)

evalList :: Show a => (Int, ListR a) -> [Int] -> (Int, ListR a)
evalList (skip, o) = foldl' eval (skip,o)

input :: [Int]
input = map ord "76,1,88,148,166,217,130,0,128,254,16,2,130,71,255,229" ++ [17, 31, 73, 47, 23]
--input = [76,1,88,148,166,217,130,0,128,254,16,2,130,71,255,229]

testInput :: String -> [Int]
testInput s = map ord s ++ [17, 31, 73, 47, 23]

passes :: [Int] -> Int -> (Int, ListR Int)
passes i n = last $ take (n+1) $ iterate (`evalList` i) (0, ListR [] 0 [1..255])

sparseHash :: [Int]
sparseHash = toL $ snd $ passes input 64

denseHash :: [Int] -> [Int]
denseHash [] = []
denseHash l = if length chunk < 16
  then error "wrong size"
  else foldl' xor 0 chunk : denseHash (drop 16 l)
  where
  chunk =  take 16 l

hashSrt :: [Int] -> String
hashSrt = concatMap (printf "%02x")

main :: IO ()
main = undefined
