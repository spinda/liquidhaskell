{-# LANGUAGE QuasiQuotes #-}

module Test0 () where

import LiquidHaskell

type Nat = [lq| {v:Int | 0 <= v} |]

ok :: Nat -> Nat
ok x = x

