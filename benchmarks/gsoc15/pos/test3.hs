{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}

module Test3 () where

import LiquidHaskell

[lq| type Nat       = { v:Int | 0 <= v } |]
[lq| type NatGE {x} = { v:Nat | x <  v } |]

ok :: [lq| x:Nat -> NatGE {x} |]
ok x = plus x one

plus :: [lq| x:Int -> y:Int -> { v:Int | v = x + y } |]
plus = undefined

one :: [lq| { v:Int | v = 1 } |]
one = undefined

