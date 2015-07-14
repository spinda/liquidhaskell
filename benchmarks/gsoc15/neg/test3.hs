{-# LANGUAGE QuasiQuotes #-}

module Test3 () where

import LiquidHaskell

import GHC.Types

[lq| type Nat     = { v:Int | 0 <= v } |]
[lq| type NatGE X = { v:Nat | X <  v } |]

bad :: [lq| x:Nat -> NatGE {x} |]
bad x = x - 1

