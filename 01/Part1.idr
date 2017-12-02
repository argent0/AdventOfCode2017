module Main

parseInput : String -> List Int
parseInput =
  filter (\x => (x>=0) && (x<=9)) . map (\x => ord x - ord '0') . unpack

matchPrevDigit : Int -> List Int -> List Int
matchPrevDigit next [] = []
matchPrevDigit next (x::xs) =
  if next == x
     then (x :: matchPrevDigit x xs)
     else matchPrevDigit x xs

safeHead : List a -> Maybe a
safeHead [] = Nothing
safeHead (x::_) = Just x

solution : List Int -> Maybe Int
solution l = safeHead l >>= \h =>
  pure $ sum $ matchPrevDigit h (reverse l)

main : IO ()
main = do
  input <- readFile "input.txt"
  case input of
    Right str => print $ solution $ parseInput str
    Left err => putStrLn "Errors were made"
