
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| myDiv :: x:Int -> y:{Int | y != 0} -> {v:Int | v = x / y} |]
myDiv :: Int -> Int -> Int
myDiv = div
