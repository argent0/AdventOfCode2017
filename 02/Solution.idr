module Main

%default total

Input : Type
Input = List (List Int)

Solution : Type
Solution = Int

parseInput : String -> Input
parseInput = map ( (map cast) . words ) . lines

mymaxfold : Ord a => a -> Maybe a -> Maybe a
mymaxfold e Nothing = Just e
mymaxfold e (Just a) = Just $ max a e

mymax : Ord a => List a -> Maybe a
mymax = foldr mymaxfold Nothing

myminfold : Ord a => a -> Maybe a -> Maybe a
myminfold e Nothing = Just e
myminfold e (Just a) = Just $ min a e

mymin : Ord a => List a -> Maybe a
mymin = foldr myminfold Nothing

solFold : Ord a => a -> Maybe (a,a) -> Maybe (a,a)
solFold e Nothing = Just (e, e)
solFold e (Just (mx, mn)) = Just $ (max mx e, min mn e)

diffOp : Neg ty => ty -> ty -> ty
diffOp a b = a - b

maxDiff : (Neg a, Ord a) => List a -> Maybe a
maxDiff l = (uncurry diffOp) <$> foldr solFold Nothing l

plusOp : Num ty => Maybe ty -> Maybe ty -> Maybe ty
plusOp Nothing Nothing = Nothing
plusOp Nothing b = b
plusOp a Nothing = a
plusOp (Just a) (Just b) = Just $ a + b

solution : Input -> Maybe Solution
solution = foldr plusOp Nothing . map maxDiff

-- nathalf : Nat -> Nat
-- nathalf Z = Z
-- nathalf (S Z) = Z
-- nathalf (S (S n)) = S (nathalf n)
-- 
-- f : Int -> Int -> Int
-- f a b = if a == b then b else 0
-- 
-- solution : Nat -> List Int -> Maybe Int
-- solution n [] = Nothing
-- solution n (x::xs) = Just $
--   sum $ zipWith f
--     (x::xs)
--     (take (length (x::xs)) (drop n (cycle (x::xs))))

main : IO ()
main = do
  inputResult <-  (parseInput <$>) <$> (readFile "input.txt")
  case inputResult of
    Right input => do
        putStrLn $ "Input: " ++ (show input)
        -- putStr "Solution Part 1: "
        -- printLn (solution (S Z) input)
        -- putStr "Solution Part 2: "
        -- printLn (solution (nathalf len) input)
    Left err =>
        putStrLn "Errors where made."
    
