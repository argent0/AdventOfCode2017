module Main where

import Data.Graph

import Debug.Trace (trace)

type Id = Integer
data Link = Link Id [Id] deriving Show

parseLink :: String -> Either String Link
parseLink str = case words str of
  root:"<->":childs ->
    Right $ Link (read root) $ map (read . filter (/=',')) childs
  _ -> Left $ "Malformed link: " ++ str

parseInput :: String -> Either String [Link]
parseInput input = mapM parseLink (lines input)

main :: IO ()
main =
  -- (parseLink <$> readFile "input.txt") >>=
  readFile "input.txt" >>= return . parseInput >>=
  print
