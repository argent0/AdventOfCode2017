module Main

%default total

parseInput : String -> List Int
parseInput = 
  filter (\x => (x>=0) && (x<=9)) . map (\x => ord x - ord '0') . unpack

nathalf : Nat -> Nat
nathalf Z = Z
nathalf (S Z) = Z
nathalf (S (S n)) = S (nathalf n)

f : Int -> Int -> Int
f a b = if a == b then b else 0

solution : Nat -> List Int -> Maybe Int
solution n [] = Nothing
solution n (x::xs) = Just $
  sum $ zipWith f
    (x::xs)
    (take (length (x::xs)) (drop n (cycle (x::xs))))

main : IO ()
main = do
  inputResult <-  (parseInput <$>) <$> (readFile "input.txt")
  case inputResult of
    Right input => do
        let len = length input
        putStrLn $ "Input Length 1: " ++ (show len)
        putStr "Solution Part 1: "
        printLn (solution (S Z) input)
        putStr "Solution Part 2: "
        printLn (solution (nathalf len) input)
    Left err => putStrLn "Errors were made"
