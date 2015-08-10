{-# LANGUAGE QuasiQuotes #-}

module Test1 () where

import LiquidHaskell

type Nat = [lq| { v:Int | 0 <= v } |]

ok :: Nat -> Nat
ok x = x + 1

