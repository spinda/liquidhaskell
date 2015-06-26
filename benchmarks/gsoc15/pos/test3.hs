{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test3 () where

import LiquidHaskell

[lq| type Nat     = { v:Int | 0 <= v } |]
[lq| type NatGE X = { v:Nat | X <  v } |]

ok :: [lq| x:Nat -> NatGE {x} |]
ok x = plus x one

plus :: [lq| x:Int -> y:Int -> { v:Int | v = x + y } |]
plus = undefined

one :: [lq| { v:Int | v = 1 } |]
one = undefined

