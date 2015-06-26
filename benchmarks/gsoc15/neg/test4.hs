{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test3 () where

import LiquidHaskell

[lq| type Nat        = { v:Int | 0 <= v } |]
[lq| type NatGE  a X = { v:a | X <  v } |]
[lq| type NatGE2   X = NatGE Nat X |]

bad :: [lq| x:Nat -> NatGE2 {x} |]
bad x = minus x one

minus :: [lq| x:Int -> y:Int -> { v:Int | v = x - y } |]
minus = undefined

one :: [lq| { v:Int | v = 1 } |]
one = undefined

