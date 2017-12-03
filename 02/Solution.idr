module Main

%default total

Input : Type
Input = List (List Nat)

data NZ : Type where
  NZC : (n : Nat) -> Not (n = Z) -> NZ

Eq NZ where
  (NZC a _) == (NZC b _) = a == b

Ord NZ where
  compare (NZC a _) (NZC b _) = compare a b

VerifiedInput : Type
VerifiedInput = List (List NZ)

Solution : Type
Solution = Nat

parseInput : String -> Input
parseInput = map ( (map cast) . words ) . lines

inputLineVerification : List Nat -> Maybe (List NZ)
inputLineVerification Nil = Just []
inputLineVerification (Z::xs) = Nothing
inputLineVerification ((S n)::xs) =
  ((NZC (S n) SIsNotZ) ::) <$> inputLineVerification xs

inputVerification : Input -> Maybe VerifiedInput
inputVerification [] = Just []
inputVerification (l::ls) = case inputLineVerification l of
                                 Nothing => Nothing
                                 Just vl => (vl ::) <$> inputVerification ls

-- solFold : Ord a => a -> Maybe (a,a) -> Maybe (a,a)
-- solFold e Nothing = Just (e, e)
-- solFold e (Just (mx, mn)) = Just $ (max mx e, min mn e)
-- 
-- diffOp : Neg ty => ty -> ty -> ty
-- diffOp a b = a - b
-- 
-- maxDiff : (Neg a, Ord a) => List a -> Maybe a
-- maxDiff l = (uncurry diffOp) <$> foldr solFold Nothing l
-- 
plusOp : Maybe Nat -> Maybe Nat -> Maybe Nat
plusOp Nothing Nothing = Nothing
plusOp Nothing b = b
plusOp a Nothing = a
plusOp (Just a) (Just b) = Just $ a + b
-- 
-- solution : Input -> Maybe Solution
-- solution = foldr plusOp Nothing . map maxDiff

partial
testLine : List NZ
testLine = (\(Just x) => x) $ inputLineVerification [5,9,2,8]

findEvenStep :  NZ -> List NZ -> Maybe (NZ, NZ)
findEvenStep c [] = Nothing
findEvenStep (NZC c pnzc) ((NZC x pnzx)::xs) =
  if (modNatNZ c x pnzx) == 0
     then Just (NZC c pnzc, NZC x pnzx)
     else findEvenStep (NZC c pnzc) xs

findEvenStep' : List NZ -> Maybe (NZ, NZ)
findEvenStep' [] = Nothing
findEvenStep' (x::xs) = findEvenStep x xs

findEvenStep'' : List NZ -> Maybe (NZ, NZ)
findEvenStep'' =
  join . find isJust . map (findEvenStep') . tails . sortBy (flip compare) 

findEvenDiv : List NZ -> Maybe Nat
findEvenDiv l = case findEvenStep'' l of
                    Just (NZC a _,NZC b pnzb) =>
                      let
                        result = (divNatNZ) a b pnzb
                      in Just $ result
                    Nothing => Nothing

solution2 : Input -> Maybe Solution
solution2 l =
  inputVerification l >>=
  pure . map findEvenDiv >>= 
  (foldr plusOp Nothing)

main : IO ()
main = do
  inputResult <-  (parseInput <$>) <$> (readFile "input.txt")
  case inputResult of
    Right input => do
        putStrLn $ "Input: " ++ (show $ solution2 input)
        -- putStr "Solution Part 1: "
        -- printLn (solution (S Z) input)
        -- putStr "Solution Part 2: "
        -- printLn (solution (nathalf len) input)
    Left err =>
        putStrLn "Errors where made."
    
