{-# LANGUAGE QuasiQuotes #-}

module Test0 () where

import LiquidHaskell

[lq| type GeNum a N = {v: a | N <= v} |]
[lq| type PosInt    = GeNum Int {0}   |]

[lq| myabs :: Int -> PosInt |]
myabs x = if (x > 0) then x else (-x)

[lq| incr :: x:Int -> GeNum Int {x} |]
incr x = x + 1

