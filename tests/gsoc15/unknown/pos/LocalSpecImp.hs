
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import LocalSpec0

[lq| bar :: {x:Int | x > 99} -> {v:Int | v > 100 } |]
bar :: Int -> Int
bar x = foo x
