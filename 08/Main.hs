module Main where

import qualified Data.Map.Strict as Map
import Data.List (elemIndex)
import qualified Control.Monad.State.Strict as S
import qualified Control.Monad.Writer.Strict as W
import Data.Maybe (fromMaybe)

newtype Max a = Max { getMax :: Maybe a } deriving Show
instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  mappend (Max Nothing) b = b
  mappend a (Max Nothing) = a
  mappend (Max (Just a)) (Max (Just b)) = Max $ Just $ max a b

type RegisterName = String
type Mem = Map.Map RegisterName Integer
type Comp = W.WriterT (Max Integer) (S.State Mem)

data PredOp = PGT | PLT | PLTE | PGTE | PEQ | PNEQ deriving Show
data Pred = Pred RegisterName PredOp Integer deriving Show
data Op = Inc | Dec deriving Show
data Expr = Expr RegisterName Op Integer deriving Show
data Instruction =
  Instruction Expr Pred
  deriving Show

parseOp :: String -> Op
parseOp "inc" = Inc
parseOp "dec" = Dec
parseOp s = error $ "Malformed Op: " ++ s

opFn :: Op -> Integer -> Integer -> Integer
opFn Inc a b = a + b
opFn Dec a b = a - b

parsePredOp :: String -> PredOp
parsePredOp pStr = case elemIndex pStr strs of
  Nothing -> error $ "Malformed predicate: " ++ pStr
  Just idx -> [ PGT , PLT , PLTE , PGTE , PEQ , PNEQ ] !! idx
  where
  strs = [ ">" , "<" , "<=" , ">=" , "==" , "!=" ]

predOpFn :: PredOp -> Integer -> Integer -> Bool
predOpFn PGT = (>)
predOpFn PLT = (<)
predOpFn PLTE = (<=)
predOpFn PGTE = (>=)
predOpFn PEQ = (==)
predOpFn PNEQ = (/=)

parseInstruction :: String -> Instruction
parseInstruction instStr = case words instStr of
  [regName, op, intVal, "if", cRegName, pred, cIntVal] ->
    Instruction
      (Expr regName (parseOp op) (read intVal))
      $ Pred cRegName (parsePredOp pred) (read cIntVal)
  _ -> error $ "Malformed input line: " ++ instStr

getReg :: RegisterName -> Comp Integer
getReg regName = fromMaybe 0 <$> S.gets (Map.!? regName)

evalPred :: Pred -> Comp Bool
evalPred (Pred regName op opArg) =
  flip (predOpFn op) opArg <$> getReg regName

doExpr :: Expr -> Comp ()
doExpr (Expr regName op opArg) =
  getReg regName >>= \regVal ->
  let
    newValue = opFn op regVal opArg
  in
    S.modify (Map.insert regName newValue) >>
    W.tell (Max $ Just newValue)

bool :: Bool -> a -> a -> a
bool True a _ = a
bool _ _ a = a
    
eval :: Instruction -> Comp ()
eval (Instruction expr pred) = do
  predResult <- evalPred pred
  if predResult
    then doExpr expr
    else pure ()

safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum [] = Nothing
safeMaximum x = Just $ maximum x

largestRegisterValue :: Comp (Maybe Integer)
largestRegisterValue = safeMaximum <$> S.gets Map.elems

main :: IO ()
main = --undefined
  do
    instructions <- (fmap parseInstruction . lines) <$> readFile "input.txt"
    print $ S.evalState ( W.runWriterT $
      S.mapM_ eval instructions >> largestRegisterValue ) Map.empty
