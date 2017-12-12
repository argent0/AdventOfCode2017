{-# LANGUAGE TupleSections #-}
module Main where

import Data.Graph as G
import Data.Tree as T

import qualified Data.List as L

type Id = Int
data Link = Link Id [Id] deriving Show

_root :: Link -> Id
_root (Link id _) = id

parseLink :: String -> Either String Link
parseLink str = case words str of
  root:"<->":childs ->
    Right $ Link (read root) $ map (read . filter (/=',')) childs
  _ -> Left $ "Malformed link: " ++ str

parseInput :: String -> Either String [Link]
parseInput input = mapM parseLink (lines input)

linkToEdges :: Link -> [G.Edge]
linkToEdges (Link root children) = map (root,) children

graph :: [Link] -> G.Graph
graph links = G.buildG (minid,maxid) $ concatMap linkToEdges links
  where
  maxid = L.maximum $ map _root links
  minid = L.minimum $ map _root links

main :: IO ()
main =
  -- (parseLink <$> readFile "input.txt") >>=
  readFile "input.txt" >>= return . parseInput >>=
  --print . (map (T.foldTree folder ) . filter ((==0) . T.rootLabel) . G.components . graph <$>)
  print . (length . G.components . graph <$>)
  where
  folder :: G.Vertex -> [Int] -> Int
  folder _ = (+1) . sum
