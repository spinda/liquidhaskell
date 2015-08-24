{-# LANGUAGE QuasiQuotes #-}

module Test0 () where

import LiquidHaskell

[lq| myabs :: x:Int -> {v: Int | if x > 0 then v = x else v + x = 0 } |]
myabs x | x > 0     = x
        | otherwise = (-x)
