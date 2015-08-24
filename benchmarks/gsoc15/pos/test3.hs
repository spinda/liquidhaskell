{-# LANGUAGE QuasiQuotes #-}

module Test3 () where

import LiquidHaskell

[lq| type Nat     = { v:Int | 0 <= v } |]
[lq| type NatGE X = { v:Nat | X <  v } |]

[lq| ok :: x:Nat -> NatGE {x} |]
ok x = x + 1

