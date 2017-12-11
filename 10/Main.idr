module Main

import Data.Vect

%default total

data Step : Nat -> Type where
  CStep : (n : Nat) -> Step n

data SplitVect : Nat -> Type -> Type where
  CSplitVect : Vect a t -> Vect b t -> SplitVect (a+b) t


svCons : a -> SplitVect n a -> SplitVect (S n) a
svCons a (CSplitVect xs ys) = CSplitVect (a::xs) ys

svRot : SplitVect n a -> SplitVect n a
svRot (CSplitVect [] []) = CSplitVect [] []
svRot (CSplitVect (x::xs) ys) = (CSplitVect xs (ys++[x]))
--svRot (CSplitVect (x::xs) ) = ?a

svIndex : Fin n -> SplitVect n a -> a
svIndex FZ (CSplitVect (x::_) _) = x
svIndex FZ (CSplitVect [] (y::ys)) = y
svIndex (FS x) (CSplitVect (_::xs) ys) = svIndex x (CSplitVect xs ys)
svIndex (FS x) (CSplitVect [] ys) = Data.Vect.index (FS x) ys

svPrep : Vect l a -> SplitVect m a -> SplitVect (l+m) a
svPrep [] sv = sv
svPrep (x::xs) sv = svCons x (svPrep xs sv)

svReplaceAt : Fin len -> a -> SplitVect len a -> SplitVect len a
svReplaceAt i y (CSplitVect [] ys) =
  CSplitVect [] (Data.Vect.replaceAt i y ys)
svReplaceAt FZ y (CSplitVect (_::xs) ys) = CSplitVect (y::xs) ys
svReplaceAt (FS i) y (CSplitVect (x::xs) ys) =
  svCons x (svReplaceAt i y (CSplitVect xs ys))

svTail : SplitVect (S len) a -> SplitVect len a
svTail (CSplitVect [] ys) = CSplitVect [] (Data.Vect.tail ys)
svTail (CSplitVect (x::xs) ys) = CSplitVect xs ys

svSwap : Fin (S n) -> Fin (S n) -> SplitVect (S n) Nat -> SplitVect (S n) Nat
svSwap a b sv =
    svReplaceAt a (svIndex b sv) (svReplaceAt b (svIndex a sv) sv)

-- svTake : (n : Nat) -> SplitVect m a -> Vect n a
-- svTake Z _ = []
-- svTake (S n) (CSplitVect (x::xs) ys)  = x :: (svTake n (CSplitVect

-- eval : {list_length : Nat} ->
--   SplitVect (S list_length) Nat ->
--   Fin (S list_length) -> --length
--   Step step_order ->
--   (Step (S step_order), SplitVect (S list_length) Nat)
-- eval list len = ?hole
       

main : IO ()
main = putStrLn "Hello, W"
