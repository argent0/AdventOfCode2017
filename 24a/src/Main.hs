module Main where

import qualified Data.Map.Strict as Map
import Data.List

type Component = (Int, Int)

compatibleComponents :: Component -> [Component] -> [Component]
compatibleComponents (a,b) = foldr folder []
  where
  folder :: Component -> [Component] -> [Component]
  folder comp@(ca,cb) acc
    | ca == a || ca == b || cb == a || cb == b = comp:acc
    | otherwise = acc

compatibleComponentMap :: Map.Map Component [Component]
compatibleComponentMap = foldr folder Map.empty indices
  where
  folder index acc =
    let
      cc = (inputComponents !! index)
      complement = map (inputComponents !!) $ filter (/=index) indices
    in Map.insert cc (compatibleComponents cc complement) acc
  indices = [0..(length inputComponents - 1)]

validConnection :: Component -> Component -> Bool
validConnection (aa,ab) (ba,bb) =
    aa == ba || aa == bb || ab == ba || ab == bb

validBridge :: [Component] -> Bool
validBridge [] = False
validBridge l@(x:xs)=
  isInitial x && and (zipWith validConnection l xs)

isInitial :: Component -> Bool
isInitial (a,b)
  | a==0 || b==0 = True
  | otherwise = False

data Tree a = Node a [Tree a] deriving Show

bridges :: [Component] -> [Tree Component]
bridges components = foldl' folder [] components
  where
  folder :: [Tree Component] -> Component -> [Tree Component]
  folder acc c =
    if isInitial c
      then Node c (nextComponents c (delete c components)):acc
      else acc

nextComponents :: Component -> [Component] -> [Tree Component]
nextComponents c cs = map mapper compatibles
  where
  mapper :: Component -> Tree Component
  mapper cc = Node cc (nextComponents cc (delete cc cs))
  compatibles = filter (validConnection c) cs

strenghts :: Tree Component -> [Int]
strenghts (Node (a,b) []) = [a+b]
strenghts (Node (a,b) cs) =
  map ((a+b)+) $ concatMap strenghts cs

main :: IO ()
main =
  print $ maximum $ concatMap  strenghts $ bridges inputComponents

testComponents :: [Component]
testComponents = [(0,2),(2,2),(2,3),(3,4),(3,5),(0,1),(10,1),(9,10)]

inputComponents :: [Component]
inputComponents = problemComponents

problemComponents :: [Component]
problemComponents = [(50,41) ,(19,43) ,(17,50) ,(32,32) ,(22,44) ,(9,39)
  ,(49,49) ,(50,39) ,(49,10) ,(37,28) ,(33,44) ,(14,14)
  ,(14,40) ,(8,40) ,(10,25) ,(38,26) ,(23,6) ,(4,16)
  ,(49,25) ,(6,39) ,(0,50) ,(19,36) ,(37,37) ,(42,26)
  ,(17,0) ,(24,4) ,(0,36) ,(6,9) ,(41,3) ,(13,3)
  ,(49,21) ,(19,34) ,(16,46) ,(22,33) ,(11,6) ,(22,26)
  ,(16,40) ,(27,21) ,(31,46) ,(13,2) ,(24,7) ,(37,45) ,(49,2)
  ,(32,11) ,(3,10) ,(32,49) ,(36,21) ,(47,47) ,(43,43) ,(27,19)
  ,(14,22) ,(13,43) ,(29,0) ,(33,36) ,(2,6)]
