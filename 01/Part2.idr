module Main

import Data.Vect

inputLength : Nat
inputLength = 2102

%default total

-- 
-- circIdx' : Vect (S len) a -> Nat -> Vect remLen a -> a
-- circIdx' ov Z Nil = Data.Vect.head ov
-- circIdx' ov Z (x::_) = x
-- circIdx' ov (S n) Nil = circIdx' ov n (Data.Vect.tail ov)
-- circIdx' ov (S n) (_::xs) = circIdx' ov n xs
-- 
-- circIdx : Vect (S len) a -> Nat -> a
-- circIdx {len} ov idx = circIdx' ov (modNatNZ idx (S len) SIsNotZ) ov
-- 
-- takeCirc : Vect (S len) a -> 
--            (n : Nat) ->
--            Vect remLen a -> Vect n a
-- takeCirc ov Z _ = Nil
-- takeCirc ov (S n) Nil = takeCirc ov (S n) ov
-- takeCirc ov (S n) (x::xs) = x :: (takeCirc ov n xs)

data Ring : Type where
  RingNil : Ring
  RingCons : List Int -> Int -> List Int -> Ring

ringCurr : Ring -> Maybe Int
ringCurr RingNil = Nothing
ringCurr (RingCons _ n _) = Just n

ringRight : Ring -> Ring
ringRight RingNil = RingNil
ringRight (RingCons [] n []) = RingCons [] n []
ringRight (RingCons (l::Nil) n []) = RingCons [n] l []
ringRight (RingCons (l::(ll::ls)) n []) = RingCons [n] (last (ll::ls)) (drop 1 (reverse (l::ll::ls)))
ringRight (RingCons l n (r::rs)) = RingCons (n::l) r rs

ringNRight : Nat -> Ring -> Ring
ringNRight Z = id
ringNRight (S n) = ringRight . (ringNRight n)

ringFromList : List Int -> Ring
ringFromList [] = RingNil
ringFromList (l::ls) = RingCons [] l ls

zipNWith : Nat -> (Int -> Int -> Int) -> Ring -> Ring -> List (Maybe Int)
zipNWith Z _ _ _ = []
zipNWith n _ RingNil RingNil = []
zipNWith (S n) f r1 r2 =
  (liftA2 f (ringCurr r1) (ringCurr r2))::(zipNWith n f (ringRight r1) (ringRight r2))

--------------------------------------------------------------------------------

parseInput : String -> Ring
parseInput = ringFromList .
  filter (\x => (x>=0) && (x<=9)) . map (\x => ord x - ord '0') . unpack


nathalf : Nat -> Nat
nathalf Z = Z
nathalf (S Z) = Z
nathalf (S (S n)) = S (nathalf n)

f : Int -> Int -> Int
f a b = if a == b then b else 0

solution : Nat -> Ring -> Int
solution length input =
  sum $ Prelude.List.catMaybes $ zipNWith length f input (ringNRight (nathalf length) input)

main : IO ()
main = do
  input <- readFile "input.txt"
  case input of
    Right str => do
        putStrLn (show (length str))
        print (solution (length str) (parseInput str))
        putStrLn ""
    Left err => putStrLn "Errors were made"
