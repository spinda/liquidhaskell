
{-# LANGUAGE QuasiQuotes #-}
[lq| LIQUID "-v" |]



import LiquidHaskell


[lq| abz :: (Num a, Ord a) => x:a -> {v: a | v >= x} |]
abz x   = if (x > 0) then x else (0 - x)  

[lq| incr :: (Num a) => x:a -> {v: a | v > x}        |]
incr x  = x + 1 

[lq| decr :: (Num a) => x:a -> {v: a | v < x}        |]
decr x  = x - 1 


