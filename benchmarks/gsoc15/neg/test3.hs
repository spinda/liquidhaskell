{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}

module Test3 () where

import LiquidHaskell

[lq| type Nat       = { v:Int | 0 <= v } |]
[lq| type NatGE {x} = { v:Nat | x <  v } |]

bad :: [lq| x:Nat -> NatGE {x} |]
bad x = minus x one

minus :: [lq| x:Int -> y:Int -> { v:Int | v = x - y } |]
minus = undefined

one :: [lq| { v:Int | v = 1 } |]
one = undefined

