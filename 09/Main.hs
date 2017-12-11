{-# LANGUAGE LambdaCase #-}
module Main where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Control.Monad (void)

newtype Group = Group [Group] deriving Show

groupScore :: Integer -> Group -> Integer
groupScore acc (Group []) = acc
groupScore acc (Group l) = acc + (sum $ map (groupScore (acc+1)) l)

data Garbage = Garbage deriving Show

garbage :: GenParser Char st (Integer, Garbage)
garbage = char '<' >> content
  where
  content = (fromIntegral . length <$> many (noneOf "!>")) >>= \gl ->
    (char '!' <|> char '>') >>=
    \case
      '!' -> anyChar >> content >>= \(acc,g) -> return (acc+gl, g)
      '>' -> return (gl, Garbage)

group :: GenParser Char st (Integer, Group)
group =
  char '{' >>
  ((char '}' >> return (0, Group [])) <|>
  (groupContent >>= \(c, inner) ->
  char '}' >> return (c, Group inner)))

groupContent :: GenParser Char st (Integer, [Group])
groupContent =
  ((garbage >>= \(gc,_) -> return (gc, [])) <|> 
  (group >>= \(gc,g) -> return (gc,[g]))) >>= \(fgc, first) ->
    (char ',' >> groupContent >>= \(gc,l) ->
      return  (gc+fgc,l++first)) <|>
    (return (fgc, first))

parseInput :: String -> Either ParseError (Integer, Group)
parseInput = parse group "(unkown)"

main :: IO ()
main = do
  fc <- readFile "input.txt"
  case parseInput fc of
    Left err -> putStrLn $ "ERROR: " ++ (show err)
    Right (gc, group) -> print $ (gc, groupScore 1 group)
