{-# LANGUAGE QuasiQuotes #-}

module Test3 () where

import LiquidHaskell

[lq| type Nat     = { v:Int | 0 <= v } |]
[lq| type NatGE X = { v:Nat | X <  v } |]

[lq| bad :: x:Nat -> NatGE {x} |]
bad x = x - 1

