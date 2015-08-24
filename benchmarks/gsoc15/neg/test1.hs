{-# LANGUAGE QuasiQuotes #-}

module Test1 () where

import LiquidHaskell

[lq| type Nat = { v:Int | 0 <= v } |]

[lq| bad :: Int -> Nat |]
bad x = x + 1

