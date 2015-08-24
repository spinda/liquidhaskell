{-# LANGUAGE QuasiQuotes #-}

module Test1 () where

import LiquidHaskell

[lq| type Nat = { v:Int | 0 <= v } |]

[lq| bad :: Nat -> Nat |]
bad x = -1 + x

