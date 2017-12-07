{-# LANGUAGE Strict #-}
module Main where

import Data.List (foldl')
import qualified Data.Array.IArray as IA
import qualified Data.Map.Strict as Map
import Control.Arrow (second)
import Data.Maybe (fromMaybe)

--import Debug.Trace (trace)

trace :: a -> b -> b
trace _ b = b

--type Memory = IA.Array Integer Integer
type Memory = [Integer]
type MemotyHash = Integer

-- Structure to keep record of the visited memory states
type MemStRec = Map.Map MemotyHash Bool

mkMemory :: [Integer] -> Memory
--mkMemory input = IA.listArray (0, fromIntegral $ length input - 1) input
mkMemory = id

memElems :: Memory -> [Integer]
memElems = id

memoryHash :: Memory -> MemotyHash
memoryHash mem = snd $ foldl' folder (0,0) (reverse $ memElems mem) 
  where
  folder :: (Integer, Integer) -> Integer -> (Integer, Integer)
  folder (exp,acc) n = (exp+1, acc+n*(100^exp))

memStep :: (Integer, [Integer]) -> [Integer] -> (Integer, [Integer])
memStep (rem, memAcc) memRem
  | rem < 0 = error "The remainder is wrong"
  | rem == 0 = (rem, reverse memAcc ++ memRem)
  | otherwise = trace ("memStep:memAcc: " ++ show (reverse memAcc) ++ show memRem) $
  case memRem of
    [] -> memStep (rem, []) (reverse memAcc)
    (x:xs) -> memStep (rem-1, (x+1):memAcc) xs

balancer :: (Integer, (Memory, MemStRec)) -> (Integer, (Memory, MemStRec))
balancer (count, (mem, memRec)) = trace (show memHsh) $
  case ((Map.!?) memRec memHsh) of
    (Just _) -> (count, (mem, memRec))
    Nothing -> case splitAtMax (memElems mem) of
      Nothing -> error "Empty memory?"
      Just (f, m:t) -> let (_, nextMem) = memStep (m, 0:reverse f) t
        in trace ("balancer: " ++ show nextMem) $
          balancer (count+1, (mkMemory nextMem, Map.insert memHsh True memRec))
  where
  memHsh = memoryHash mem

main :: IO ()
main = do
  input <- fmap (read :: String -> Integer) . words <$> readFile "input.txt"
  print $ fst $ balancer (0, (mkMemory input, Map.empty))

splitAtMax :: [Integer] -> Maybe ([Integer], [Integer])
splitAtMax = (second reverse . snd <$>) <$> foldl' folder Nothing
  where
  folder :: Maybe (Integer, ([Integer], [Integer])) -> Integer -> Maybe (Integer, ([Integer], [Integer]))
  folder Nothing n = Just (n, ([], [n]))
  folder (Just (pmax, (f, t))) n = Just $
    if n > pmax
      then (n, (f ++ reverse t,[n]))
      else (pmax, (f, n:t))
