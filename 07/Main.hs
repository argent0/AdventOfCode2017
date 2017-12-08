module Main where

import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Control.Arrow
import Data.Maybe (fromMaybe)
import Data.List

import Debug.Trace (trace)

type Name = String
type Weight = Integer

data Record = Record Name Weight [Name] deriving Show

recName :: Record -> Name
recName (Record n _ _) = n

terminalP :: Record -> Bool
terminalP (Record _ _ c) = null c

newtype Fix f = In { out :: f (Fix f) }

data TreeF a f = TreeF a [f] deriving Show

instance Functor (TreeF a) where
   fmap f (TreeF r l) = TreeF r (fmap f l)

type Algebra f a = f a -> a
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = out >>> fmap (cata alg) >>> alg

treeElmsAlg :: Algebra (TreeF a) Integer
treeElmsAlg (TreeF _ []) = 1
treeElmsAlg (TreeF _ l) = 1 + sum l

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight = not . isLeft

fromLeft :: Either a b -> a
fromLeft (Left a) = a

treeUnblcdAlg :: Algebra  (TreeF Record)
                          (Either (Integer, [Integer]) String)
treeUnblcdAlg (TreeF (Record n w _) []) = Left (w,[])
treeUnblcdAlg (TreeF (Record n w _) acc) =
  if all isLeft acc
    then if pred
      then Left (w, resulWei)
      else Right (show (Record n w []) ++ (show pvs) ++ "-->" ++ show resulWei)
    else head $ filter (isRight) acc
  where
    pred = length (group resulWei) == 1
    resulWei = map (\x -> fst x + sum (snd x)) pvs
    pvs = map fromLeft acc

treeStrAlg :: Algebra (TreeF Record) String
treeStrAlg (TreeF (Record n w []) []) = n++"("++show w++")"
treeStrAlg (TreeF (Record n w _) strs) =
  heading ++ "\n" ++
    unlines (
    filter (not . null) $
    lines $
    unlines  $ map (unlines . map (replicate 2 ' '++) . lines) strs)
  where
  heading = n++"("++show w++")"
  lh = length heading

type Tree = (Fix (TreeF Record))
treeC :: Record -> [Tree] -> Tree
treeC r l = In $ TreeF r l

type TreeBlocksMap = Map.Map Name Tree
type RecMap = Map.Map Name Record

buildTree ::TreeBlocksMap -> RecMap -> Name -> (TreeBlocksMap, Tree)
buildTree blocks recMap name = case (Map.!?) blocks name of
    Just tree -> (blocks, tree)
    Nothing ->
      let
        rec@(Record _ w ch) = (Map.!) recMap name
        (nextBlocks, childrenTrees) = foldl' folder (blocks,[]) ch
        newTree = treeC rec childrenTrees
      in (Map.insert name newTree nextBlocks, newTree)
  where
  folder :: (TreeBlocksMap, [Tree]) -> Name -> (TreeBlocksMap, [Tree])
  folder (blks, tAcc) chNam =
    let
      (nBlks, nT) = buildTree blks recMap chNam
    in (nBlks, nT:tAcc)
  
parseRec :: String -> Record
parseRec recStr = case words recStr of
  [n,w] -> Record n (read w) []
  (n:w:"->":ch) -> Record n (read w) $ map (filter (/=',')) ch
  _ -> error "Malformed input line"

parseInput :: String -> [Record]
parseInput input = map parseRec $ lines input

fullTree :: TreeBlocksMap -> RecMap -> Integer -> [Record] -> Maybe Tree
fullTree tBlocks recMap len [] = Nothing
fullTree tBlocks recMap len (n:ns) = --trace (show treeLen) $
  if treeLen == len
    then Just tree
    else fullTree nBlks recMap len ns
  where
  (nBlks, tree) = buildTree tBlocks recMap (recName n)
  treeLen = cata treeElmsAlg tree

main :: IO ()
main = do
  recs <- parseInput <$> readFile "input.txt"
  let nRecs = length recs
  print nRecs
  let fulltree = fullTree Map.empty (Map.fromList $ map (recName &&& id) recs) (fromIntegral nRecs) recs
  fromMaybe (putStr "Not Found\n") $ do
    --putStr <$> cata treeStrAlg <$> fulltree
    print <$> cata treeUnblcdAlg <$> fulltree
  --
  --putStr $ cata treeStrAlg $ snd $ buildTree Map.empty (Map.fromList $ map (recName &&& id) recs) "tknk"
