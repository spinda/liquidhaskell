
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude (liquidAssert)

[lq| (!) :: zogbert:{v: Int | v > 0} -> {v: Int | v > 0} -> Int |]
(!) :: Int -> Int -> Int
x ! y = x + y


[lq| (!!) :: {v: Int | v > 0} -> {v: Int | v > 0} -> Int |]
(!!)   :: Int -> Int -> Int 
x !! y = liquidAssert (x /= 0) $ x + y 

[lq| zoo :: {v: Int | v > 0} -> {v: Int | v > 0} -> Int |]
zoo   :: Int -> Int -> Int 
zoo x y = liquidAssert (x /= 0) $ x + y 


