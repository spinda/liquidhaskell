{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test3 () where

import LiquidHaskell

[lq| type Nat     = {v:Int | 0 <= v} |]
[lq| type NatGE X = {v:Nat | 1 <  v} |]

bad :: [lq| x:Nat -> NatGE |]
bad x = x - 1

