
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| isEven :: n:Nat -> Bool / [n, 0]|]
[lq| isOdd  :: m:Nat -> Bool / [m, 1] |]
isEven, isOdd  :: Int -> Bool

isEven 0 = True
isEven n = isOdd  $ n - 1

isOdd  k = not $ isEven k



