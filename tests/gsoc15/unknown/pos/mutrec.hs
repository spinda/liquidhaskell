
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| isEven :: Nat -> {v:Int | v = 0} -> Bool |]
[lq| Decrease isEven 1 2 |]
isEven :: Int -> Int -> Bool
isEven 0 _ = True
isEven n _ = isOdd (n-1) 1

[lq| isOdd :: Nat -> {v:Int | v = 1} -> Bool |]
[lq| Decrease isOdd 1 2 |]
isOdd :: Int -> Int -> Bool
isOdd  n _ = not $ isEven n 0
