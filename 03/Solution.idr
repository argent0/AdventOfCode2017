module Main

import Debug.Trace

-- The greatest number in the n-th ring
ringMax : Integer -> Integer
ringMax n = pow (2*n+1) 2

ringSideSize : Integer -> Integer
ringSideSize n = 2*n+1

ring : Integer -> Integer
ring n = cast $ ceiling $ (sqrt (cast n) - 1) / 2

ringSide : Integer -> Integer
ringSide n = mod (ringMax (ring n) - n) (ringSideSize (ring n))
