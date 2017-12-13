{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad
import Debug.Trace (trace)
import Data.List (foldl')
import Control.Arrow

newtype Layer = Layer Integer  deriving Show
_range :: Layer -> Integer
_range (Layer r) = r

scannerPosition :: Layer -> Integer -> Integer
scannerPosition (Layer range) tick
  | range > 1 = (range-1) - abs(tick `mod` (2*(range-1)) - (range-1))
  | otherwise = error "Layer with range less than two"

newtype Firewall = Firewall [Maybe Layer] deriving Show

-- #step == layer
-- Returns a list of whether the scanner got you
part1 ::Integer -> Firewall -> [Bool]
part1 delay (Firewall layers) = zipWith zipper layers [delay..]
  where
  zipper :: Maybe Layer -> Integer -> Bool
  zipper Nothing _ = False
  zipper (Just layer) tick = scannerPosition layer tick == 0

main :: IO ()
main = do
  parseResult <- parseInput <$> readFile "input.txt"
  case parseResult of
    Left err -> print err
    Right firewall ->
      print $ head $
      filter (all not . snd) $
      map (mapper firewall) [0..]
  where
  mapper :: Firewall -> Integer -> (Integer, [Bool])
  mapper firewall delay = (delay, part1 delay firewall)

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
