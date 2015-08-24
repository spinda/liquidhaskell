{-# LANGUAGE QuasiQuotes #-}

module Test0 () where

import LiquidHaskell

[lq| type Nat = { v:Int | 0 <= v } |]

[lq| ok :: Nat -> Nat |]
ok x = hi
  where
    hi :: [lq| Nat |]
    hi = x

