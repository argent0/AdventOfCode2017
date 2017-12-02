module Main

%default total

Input : Type
Input = List (List Int)

Solution : Type
Solution = Int

parseInput : String -> Input
parseInput = map ( (map cast) . words ) . lines

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

main : IO ()
main = do
  inputResult <-  (parseInput <$>) <$> (readFile "input.txt")
  case inputResult of
    Right input => do
        putStrLn $ "Input: " ++ (show $ solution input)
        -- putStr "Solution Part 1: "
        -- printLn (solution (S Z) input)
        -- putStr "Solution Part 2: "
        -- printLn (solution (nathalf len) input)
    Left err =>
        putStrLn "Errors where made."
    
