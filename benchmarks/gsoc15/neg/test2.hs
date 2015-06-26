{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test0 () where

import LiquidHaskell

[lq| embed Int as int |]

type Nat = [lq| { v:Int | 0 <= v } |]

ok :: Nat -> Nat
ok x = hi
  where
    hi :: Nat
    hi = -1

