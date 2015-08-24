{-# LANGUAGE QuasiQuotes #-}

module Test0 () where

import LiquidHaskell

[lq| myabs :: x:Int -> {v: Int | v = if x > 0 then x else -x } |]
myabs x | x > 0     = x
        | otherwise = (-x)

