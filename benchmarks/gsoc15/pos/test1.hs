{-# LANGUAGE QuasiQuotes #-}

module Test1 () where

import LiquidHaskell

[lq| type Nat = { v:Int | 0 <= v } |]

[lq| ok :: Nat -> Nat |]
ok x = x + 1

