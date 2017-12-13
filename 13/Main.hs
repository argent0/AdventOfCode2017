{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad
import Debug.Trace (trace)
import Data.List (foldl')
import Control.Arrow

-- Layer Range
newtype Layer = Layer Integer  deriving Show
_range :: Layer -> Integer
_range (Layer r) = r

scannerPosition :: Layer -> Integer -> Integer
scannerPosition (Layer range) tick
  | range > 1 = (range-1) - abs(tick `mod` (2*(range-1)) - (range-1))
  | otherwise = error "Layer with range less than two"

newtype Firewall = Firewall [Maybe Layer] deriving Show

data HitchState = HitchState Integer Integer [Integer]
_tick :: HitchState -> Integer
_tick (HitchState t _ _) = t

_depth :: HitchState -> Integer
_depth (HitchState _ d _) = d

_cost :: HitchState -> [Integer]
_cost (HitchState _ _ c) = c

part1 :: Firewall -> HitchState -> [Integer]
part1 (Firewall layers) initialHitchState = _cost $ foldl' folder initialHitchState layers
  where
  folder :: HitchState -> Maybe Layer -> HitchState
  folder (HitchState t d c) Nothing = HitchState (succ t) (succ d) c
  folder (HitchState t d c) (Just layer) = HitchState (succ t) (succ d) $
    (if scannerPosition layer t == 0
      then 1 -- d*_range layer
      else 0):c

main :: IO ()
main = do
  parseResult <- parseInput <$> readFile "test.1.txt"
  case parseResult of
    Left err -> print err
    Right firewall ->
      --print $ part1 firewall (HitchState 0 0 [])
      print $ head $
      filter (all (==0) . snd) $
      map (mapper firewall) [0..]
  where
  mapper :: Firewall -> Integer -> (Integer, [Integer])
  mapper firewall delay = (delay, part1 firewall $ HitchState delay 0 [])

parseInput :: String -> Either String Firewall
parseInput input = Firewall . reverse . snd . foldl' folder (-1,[]) <$> mapM parseLine (lines input)
  where
  folder :: (Integer, [Maybe Layer]) -> (Integer, Integer) -> (Integer, [Maybe Layer])
  folder (prevDepth, acc) (depth, range) = (depth,) $
    (++acc) $ Just (Layer range):
    (if depth == prevDepth + 1
      then []
      else replicate (fromIntegral $ depth - prevDepth - 1) Nothing)
  parseLine :: String -> Either String (Integer, Integer)
  parseLine line = case words (filter (/=':') line) of
    [depth, range] -> Right (read depth, read range)
    _ -> Left $ "Error parsing line: " ++ line
